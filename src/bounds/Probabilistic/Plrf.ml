open! OurBase
open Formulas
open Constraints
open Atoms
open Polynomials
open ProbabilisticProgramModules
module Valuation = Valuation.Make (OurRational)

module CoeffTableEntry = struct
  module Inner = struct
    type t = Location.t * Var.t [@@deriving ord, sexp]

    let hash = Hashtbl.hash
  end

  include Inner
  include Comparator.Make (Inner)
end

type coeffs_table_t = (CoeffTableEntry.t, Var.t) Hashtbl.t
type template_table_t = (Location.t, RationalParameterPolynomial.t) Hashtbl.t

type t = {
  rank : Location.t -> RationalPolynomial.t;
  (* The ranked transition itself *)
  decreasing : GeneralTransition.t;
  non_increasing : GeneralTransitionSet.t;
}

let to_string t =
  "{decreasing: "
  ^ GeneralTransition.to_id_string t.decreasing
  ^ " non_incr: "
  ^ GeneralTransitionSet.to_id_string t.non_increasing
  ^ " rank: ["
  ^ (GeneralTransitionSet.locations t.non_increasing
    |> Set.to_list
    |> List.map ~f:(fun loc -> Location.to_string loc ^ ": " ^ (t.rank loc |> RationalPolynomial.to_string))
    |> String.concat ~sep:"; ")
  ^ "]}"


let rank t = t.rank
let decreasing t = t.decreasing
let non_increasing t = Set.union t.non_increasing (Set.singleton (module GeneralTransition) t.decreasing)

type lrsm_cache = { template_table : template_table_t; coeffs_table : coeffs_table_t }

let new_cache () =
  {
    template_table = Hashtbl.create ~size:10 (module Location);
    coeffs_table = Hashtbl.create ~size:10 (module CoeffTableEntry);
  }


let logger = Logging.(get PLRF)

(* Encode the expected update as parameter polynomial. *)
let as_realparapoly label var =
  Option.value ~default:(UpdateElement.of_var var) (TransitionLabel.update label var)
  |> UpdateElement.exp_value_poly |> RationalParameterPolynomial.of_polynomial


(** Given a list of variables an affine template-parameter-polynomial is generated*)
let ranking_template cache location (vars : VarSet.t) : RationalParameterPolynomial.t * Var.t list * Var.t =
  let vars = Set.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Real num_vars in
  let fresh_coeffs = List.map ~f:RationalPolynomial.of_var fresh_vars in

  (* Store fresh_vars *)
  List.iter
    ~f:(fun (v, v') -> Hashtbl.add_exn cache.coeffs_table ~key:(location, v) ~data:v')
    (List.zip_exn vars fresh_vars);

  let linear_poly = RationalParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Real () in
  let constant_poly = RationalParameterPolynomial.of_constant (RationalPolynomial.of_var constant_var) in
  (RationalParameterPolynomial.(linear_poly + constant_poly), fresh_vars, constant_var)


(* This will store the added coefficients and constants for later optimisation *)
let fresh_coeffs : Var.t list ref = ref []
let fresh_consts : Var.t list ref = ref []

(* use ranking_template to compute ranking_templates for every location *)
let compute_ranking_templates cache (vars : VarSet.t) (locations : Location.t list) : unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let parameter_poly, fresh_vars, fresh_const = ranking_template cache location vars in
      (location, parameter_poly, fresh_vars, fresh_const)
    in
    let templates = List.map ~f:ins_loc_prf locations in
    templates
    |> List.iter ~f:(fun (location, polynomial, _, _) ->
        Hashtbl.add_exn cache.template_table ~key:location ~data:polynomial);
    templates
    |> List.fold_left
         ~f:(fun (l_vars, l_consts) (_, _, fresh_vars, fresh_const) ->
           (List.cons fresh_vars l_vars, List.cons fresh_const l_consts))
         ~init:([], [])
    |> Tuple2.map1 List.join
    |> fun (fresh_vars, fresh_cs) ->
    fresh_coeffs := fresh_vars;
    fresh_consts := fresh_cs
  in
  Logger.with_log logger Logger.DEBUG
    (fun () -> ("compute_ranking_templates", []))
    ~result:(fun () ->
      Hashtbl.to_sequence cache.template_table
      |> Util.sequence_to_string ~f:(fun (location, polynomial) ->
          Location.to_string location ^ ": " ^ RationalParameterPolynomial.to_string polynomial))
    execute


(* Combine the update templates with probabilistic branching *)
let expected_poly cache gtrans =
  (* parapoly for one branch *)
  let prob_branch_poly cache (l, t, l') =
    let template = Hashtbl.find_exn cache.template_table in
    let prob = t |> TransitionLabel.probability in
    RationalParameterPolynomial.mul
      (prob |> RationalParameterPolynomial.of_polynomial)
      (RationalParameterPolynomial.substitute_f (as_realparapoly t) (template l'))
  in
  Set.fold
    ~f:(fun poly trans -> RationalParameterPolynomial.add (prob_branch_poly cache trans) poly)
    (gtrans |> GeneralTransition.transitions)
    ~init:RationalParameterPolynomial.zero


(* Logically encode the update applied to templates of a single transition t.
 * New Variables representing the updates values are introduced and substituted into the template.
 * The update constraints are then encoded by corresponding constraints on the fresh variables *)
let encode_update cache tguard (t : Transition.t) =
  let start_template = Hashtbl.find_exn cache.template_table (Transition.target t) in
  let new_var_map =
    let vars = RationalParameterPolynomial.vars start_template in
    Set.fold
      ~f:(fun var_map old_var -> Map.add_exn var_map ~key:old_var ~data:(Var.fresh_id Var.Int ()))
      vars
      ~init:(Map.empty (module Var))
  in
  let update_map = Transition.label t |> TransitionLabel.update_map in
  let update_guard old_var ue =
    let new_var = Map.find_exn new_var_map old_var in
    UpdateElement.as_linear_guard tguard ue new_var
  in
  let template_substituted =
    Hashtbl.find_exn cache.template_table (Transition.target t)
    |> RationalParameterPolynomial.rename (Map.to_alist new_var_map |> RenameMap.from)
  in
  let constrs =
    Map.fold
      ~f:(fun ~key ~data ->
        RationalConstraint.mk_and (RationalConstraint.of_intconstraint @@ update_guard key data))
      update_map ~init:RationalConstraint.mk_true
  in
  (constrs, template_substituted)


(* generate a RSM constraint of the specified type for a general transition *)
let general_transition_constraint cache constraint_type gtrans : RationalFormula.t =
  let template = gtrans |> GeneralTransition.src |> Hashtbl.find_exn cache.template_table in
  let lift_paraatom pa =
    ( GeneralTransition.guard gtrans |> RationalParameterConstraint.of_intconstraint,
      RationalParameterAtom.replace_nonlinear_monomials_with_temp_vars pa )
    |> List.return
  in
  let guard = GeneralTransition.guard gtrans in
  let guard_real = GeneralTransition.guard gtrans |> RationalConstraint.of_intconstraint in
  let constraints_and_atoms =
    match constraint_type with
    | `Non_Increasing -> RationalParameterAtom.Infix.(template >= expected_poly cache gtrans) |> lift_paraatom
    | `Decreasing ->
        RationalParameterAtom.Infix.(
          template
          >= RationalParameterPolynomial.(
               expected_poly cache gtrans + RationalParameterPolynomial.of_polynomial RationalPolynomial.one))
        |> lift_paraatom
    | `Bounded_For_Nonincreasing when not (GeneralTransition.is_probabilistic gtrans) -> []
    | `Bounded_For_Decreasing
    | `Bounded_For_Nonincreasing ->
        (* for every transition the guard needs to imply that after the guard is passed and the update is evaluated the template
         * evaluated at the corresponding location is non-negative *)
        let transition_bound t =
          let upd_constrs, templ_subst = encode_update cache guard t in
          ( RationalParameterConstraint.of_constraint @@ RationalConstraint.mk_and guard_real upd_constrs,
            RationalParameterAtom.Infix.(templ_subst >= RationalParameterPolynomial.zero) )
        in
        GeneralTransition.transitions gtrans |> Set.to_list |> List.map ~f:transition_bound
  in
  List.map
    ~f:(fun (c, a) ->
      RationalParameterConstraint.farkas_transform (RationalParameterConstraint.drop_nonlinear c) a)
    constraints_and_atoms
  |> List.map ~f:RationalFormula.mk
  |> List.fold_left ~f:RationalFormula.mk_and ~init:RationalFormula.mk_true


let non_increasing_constraint cache transition =
  general_transition_constraint cache `Non_Increasing transition


let bounded_for_decreasing_constraint cache transition =
  let constr = general_transition_constraint cache `Bounded_For_Decreasing transition in
  constr


let bounded_for_nonincreasing_constraint cache transition =
  let constr = general_transition_constraint cache `Bounded_For_Nonincreasing transition in
  constr


let decreasing_constraint cache transition =
  let constr = general_transition_constraint cache `Decreasing transition in
  constr


module Solver = SMT.IncrementalZ3Solver

type plrf_problem = {
  program : Program.t;
  make_non_increasing : GeneralTransitionSet.t;
  unbounded_vars : GeneralTransition.t * Location.t -> VarSet.t;
  is_time_bounded : GeneralTransition.t * Location.t -> bool;
  decreasing : GeneralTransition.t;
}

(** Searches for a set of non-increasing general transitions and a PLRF with a single SMT query.
    Every candidate [gt] gets an indicator variable in \{0,1\}, which is 1 iff [gt] is non-increasing.
    A non-increasing set is admissible if all its entry transitions are time-bounded, and the coefficients of the
    variables with unbounded expected size on an entry transition are 0 at the location where it enters. *)
let compute_plrf cache problem =
  let non_increasing_indicators =
    Set.to_list problem.make_non_increasing
    |> List.map ~f:(fun gt -> (gt, Var.fresh_id Var.Int ()))
    |> Map.of_alist_exn (module GeneralTransition)
  in
  let in_non_increasing_set gt =
    Map.find non_increasing_indicators gt
    |> Option.value_map ~default:RationalFormula.mk_false ~f:(fun indicator ->
        RationalFormula.mk_eq (RationalPolynomial.of_var indicator) RationalPolynomial.one)
  and not_in_non_increasing_set gt =
    Map.find non_increasing_indicators gt
    |> Option.value_map ~default:RationalFormula.mk_true ~f:(fun indicator ->
        RationalFormula.mk_eq (RationalPolynomial.of_var indicator) RationalPolynomial.zero)
  in
  let indicator_bounds indicator =
    RationalPolynomial.
      [ RationalFormula.mk_ge (of_var indicator) zero; RationalFormula.mk_le (of_var indicator) one ]
  in
  let transition_constraints gt =
    if GeneralTransition.equal gt problem.decreasing then
      (* The bounded constraint is needed since otherwise the ranking function could become negative after the
         evaluation of a decreasing transition *)
      [ in_non_increasing_set gt; decreasing_constraint cache gt; bounded_for_decreasing_constraint cache gt ]
    else
      [
        RationalFormula.mk_or (not_in_non_increasing_set gt)
          (RationalFormula.mk_and (non_increasing_constraint cache gt)
             (bounded_for_nonincreasing_constraint cache gt));
      ]
  in
  let zero_coefficients_at entry_loc entry_gt_and_loc =
    Set.to_list (problem.unbounded_vars entry_gt_and_loc)
    |> List.map ~f:(fun v ->
        RationalFormula.mk_eq
          (RationalPolynomial.of_var @@ Hashtbl.find_exn cache.coeffs_table (entry_loc, v))
          RationalPolynomial.zero)
    |> RationalFormula.all
  in
  let entry_constraints gt =
    let entry_loc = GeneralTransition.src gt in
    Set.to_list (Program.pre_gt problem.program gt)
    |> List.map ~f:(fun pre ->
        if problem.is_time_bounded (pre, entry_loc) then
          RationalFormula.any
            [
              not_in_non_increasing_set gt;
              in_non_increasing_set pre;
              zero_coefficients_at entry_loc (pre, entry_loc);
            ]
        else
          RationalFormula.mk_or (not_in_non_increasing_set gt) (in_non_increasing_set pre))
  in
  (* The constraints are added separately, as their conjunction would be expanded into a large DNF *)
  let solver = Solver.create () in
  List.concat_map ~f:indicator_bounds (Map.data non_increasing_indicators)
  @ List.concat_map
      ~f:(fun gt -> transition_constraints gt @ entry_constraints gt)
      (Set.to_list problem.make_non_increasing)
  |> List.iter ~f:(Solver.add_real solver);
  Solver.model_real solver
  |> Option.map ~f:(fun model ->
      let is_non_increasing gt =
        OurRational.equal OurRational.one
          (Valuation.eval_opt (Map.find_exn non_increasing_indicators gt) model |? OurRational.zero)
      in
      let rfunc loc =
        Hashtbl.find_exn cache.template_table loc
        |> RationalParameterPolynomial.eval_coefficients (fun var ->
            Valuation.eval_opt var model |? OurRational.zero)
      in
      {
        decreasing = problem.decreasing;
        non_increasing = Set.filter problem.make_non_increasing ~f:is_non_increasing;
        rank = rfunc;
      })


(* Compute a ranking function as a side effect such that the given transition is decreasing.
 * The computed ranking function is subsequently added to the RankingTable cache *)
let find_plrf program is_time_bounded unbounded_vars vars locations trans decreasing =
  let cache = new_cache () in
  compute_ranking_templates cache vars locations;
  compute_plrf cache { program; is_time_bounded; unbounded_vars; decreasing; make_non_increasing = trans }


let plrf_option_to_string = function
  | Some a -> to_string a
  | None -> "None"


let find_scc program is_time_bounded unbounded_vars scc gt =
  let execute () =
    let vars =
      Set.to_sequence scc
      |> Sequence.map ~f:GeneralTransition.input_vars
      |> Sequence.fold ~f:Set.union ~init:VarSet.empty
    in
    let locations = GeneralTransitionSet.locations scc |> Set.to_list in
    find_plrf program is_time_bounded unbounded_vars vars locations scc gt
    |> tap (function
      | Some r -> Logger.log logger Logger.INFO (fun () -> ("found_plrf", [ ("plrf", to_string r) ]))
      | None ->
          Logger.log logger Logger.WARN (fun () -> ("no_plrf", [ ("gt", GeneralTransition.to_id_string gt) ])))
  in

  Logger.with_log logger Logger.DEBUG
    (fun () ->
      ( "find_scc",
        [ ("decreasing:", GeneralTransition.to_id_string gt); ("refined", "TODO" (*Bool.to_string refined*)) ]
      ))
    ~result:plrf_option_to_string execute


(** Compute a ranking function such that the given gt is decreasing and return it. *)
let find program =
  let execute () =
    Sequence.of_list (Program.sccs_gts program)
    |> Sequence.map ~f:(fun scc ->
        let is_time_bounded (gt, _) = not (Set.mem scc gt) in
        let unbounded_vars (gt, _) =
          if Set.mem scc gt then
            Program.vars program
          else
            VarSet.empty
        in
        Set.to_sequence scc
        |> Sequence.map ~f:(fun gt -> (gt, find_scc program is_time_bounded unbounded_vars scc gt)))
    |> Sequence.join |> Sequence.memoize
  in
  Logger.with_log logger Logger.DEBUG
    (fun () -> ("find", [ ("refined", "TODO" (*Bool.to_string refined*)) ]))
    ~result:
      (Util.sequence_to_string ~f:(fun (gt, plrfo) ->
           GeneralTransition.to_id_string gt ^ ": " ^ plrf_option_to_string plrfo))
    execute
  |> Sequence.filter_opt % Sequence.map ~f:Tuple2.second


let compute_proof t bound_str program format =
  let module GP = GraphPrint.ProbabilisticGraphPrint in
  let non_incr_transs = GeneralTransitionSet.all_transitions t.non_increasing in
  let decreasing_trans = GeneralTransition.transitions t.decreasing in
  let color_map =
    Set.fold ~f:(fun cmap t -> Map.set ~key:t ~data:GP.Blue cmap) non_incr_transs ~init:GP.empty_color_map
    |> fun cmap ->
    Set.fold ~f:(fun cmap decr_trans -> Map.set ~key:decr_trans ~data:GP.Red cmap) decreasing_trans ~init:cmap
  in
  let locations = TransitionSet.locations non_incr_transs |> Set.to_list in
  FormattedString.(
    mk_header_small (mk_str ("Plrf for transition " ^ GeneralTransition.to_string_pretty t.decreasing ^ ":"))
    <> mk_paragraph (mk_str "new bound:" <> mk_newline <> mk_paragraph (mk_str bound_str))
    <> mk_str "PLRF:" <> mk_newline
    <> mk_paragraph
         (locations
         |> List.map ~f:(fun l ->
             "• " ^ Location.to_string l ^ ": " ^ RationalPolynomial.to_string_pretty (t.rank l))
         |> List.map ~f:mk_str_line |> mappend)
    <>
    match format with
    | Formatter.Html -> FormattedString.mk_raw_str (GP.print_system_pretty_html ~color_map program)
    | _ -> FormattedString.Empty)
