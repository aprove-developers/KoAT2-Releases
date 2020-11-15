open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open Valuation

module Valuation = Valuation.Make(OurFloat)

module RankingTable = Hashtbl.Make(struct include GeneralTransition let equal = GeneralTransition.same
                                                                    let hash = Hashtbl.hash % GeneralTransition.id end)

module TemplateTable = Hashtbl.Make(Location)

type t = {
    rank : Location.t -> RealPolynomial.t;
    (* The ranked transition itself *)
    decreasing : GeneralTransition.t;
    non_increasing : GeneralTransitionSet.t;
  }

let rank t = t.rank
let decreasing t = t.decreasing
let non_increasing t = GeneralTransitionSet.union t.non_increasing (GeneralTransitionSet.singleton t.decreasing)

type lrsm_cache = ((t option) RankingTable.t * RealParameterPolynomial.t TemplateTable.t)

let get_ranking_table   (cache: lrsm_cache) = Tuple2.first cache
let get_template_table  (cache: lrsm_cache) = Tuple2.second cache

let new_cache () : lrsm_cache = RankingTable.create 10, TemplateTable.create 10

let logger = Logging.(get LexRSM)

type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded | `Bounded_Refined ] [@@deriving show, eq]

(* Encode the update as parameter polynomial *)
let as_realparapoly label var =
  match TransitionLabel.update label var with
  | None -> RealParameterPolynomial.of_var var
  | Some (TransitionLabel.UpdateElement.Poly p) -> p |> RealPolynomial.of_intpoly |> RealParameterPolynomial.of_polynomial
  | Some (TransitionLabel.UpdateElement.Dist d) ->
      ProbDistribution.expected_value d
      |> RealPolynomial.add (RealPolynomial.of_var var)
      |> RealParameterPolynomial.of_polynomial

(** Given a list of variables an affine template-parameter-polynomial is generated*)
let ranking_template (vars: VarSet.t): RealParameterPolynomial.t * Var.t list * Var.t =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Real num_vars in
  let fresh_coeffs = List.map RealPolynomial.of_var fresh_vars in
  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Real () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly), fresh_vars,constant_var

(* This will store the added coefficients and constants for later optimisation *)
let fresh_coeffs: Var.t list ref = ref []
let fresh_consts: Var.t list ref = ref []

(* use ranking_template to compute ranking_templates for every location *)
let compute_ranking_templates cache (vars: VarSet.t) (locations: Location.t list): unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars, fresh_const) = ranking_template vars in
      (location, parameter_poly, fresh_vars, fresh_const)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_,_) -> TemplateTable.add (get_template_table cache) location polynomial);
    templates
    |> List.fold_left (fun (l_vars,l_consts) (_,_,fresh_vars,fresh_const) -> (List.cons fresh_vars l_vars,List.cons fresh_const l_consts)) ([],[])
    |> Tuple2.map1 List.flatten
    |> (fun (fresh_vars, fresh_cs) -> fresh_coeffs := fresh_vars; fresh_consts := fresh_cs)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_ranking_templates", [])
                  ~result:(fun () ->
                    get_template_table cache
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ RealParameterPolynomial.to_string polynomial)
                  )
                  execute

(* Combine the update templates with probabilistic branching *)
let expected_poly cache gtrans =
  (* parapoly for one branch *)
  let prob_branch_poly cache (l,t,l') =
      let template = (fun key -> key |> TemplateTable.find (get_template_table cache)) in
      let prob = t |> TransitionLabel.probability in
      RealParameterPolynomial.mul
        (prob |> RealPolynomial.of_constant |> RealParameterPolynomial.of_polynomial)
        (RealParameterPolynomial.substitute_f (as_realparapoly t) (template l'))
  in
  TransitionSet.fold
    (fun trans poly -> RealParameterPolynomial.add (prob_branch_poly cache trans) poly)
    (gtrans |> GeneralTransition.transitions) RealParameterPolynomial.zero

(* Logically encode the update applied to templates of a single transition t.
 * New Variables representing the updates values are introduced and substituted into the template.
 * The update constraints are then encoded by corresponding constraints on the fresh variables *)
let encode_update cache t =
  let start_template =
    TemplateTable.find (get_template_table cache) (Transition.target t)
  in
  let new_var_map =
    let vars = RealParameterPolynomial.vars start_template in
    VarSet.fold (fun old_var var_map -> TransitionLabel.VarMap.add old_var (Var.fresh_id Var.Int ()) var_map) vars TransitionLabel.VarMap.empty
  in
  let update_map = Transition.label t |> TransitionLabel.update_map in
  let update_guard old_var ue =
    let new_var = TransitionLabel.VarMap.find old_var new_var_map in
    match ue with
    | TransitionLabel.UpdateElement.Poly p ->
        Constraint.mk_eq (Polynomial.of_var new_var) p |> RealConstraint.of_intconstraint
    | TransitionLabel.UpdateElement.Dist d ->
      ProbDistribution.guard d old_var new_var |> RealConstraint.of_intconstraint
  in
  let template_substituted =
    TemplateTable.find (get_template_table cache) (Transition.target t)
    |> RealParameterPolynomial.rename (TransitionLabel.VarMap.bindings new_var_map |> RenameMap.from)
  in
  let constrs =
    TransitionLabel.VarMap.fold (fun old_var ue -> RealConstraint.mk_and (update_guard old_var ue)) update_map RealConstraint.mk_true
  in
  constrs, template_substituted


(* generate a RSM constraint of the specified type for a general transition *)
let general_transition_constraint cache constraint_type gtrans: RealFormula.t =
  let template = gtrans |> GeneralTransition.start |> TemplateTable.find (get_template_table cache) in
  let lift_paraatom pa = (GeneralTransition.guard gtrans |> RealParameterConstraint.of_intconstraint,pa) |> List.singleton in
  let guard = GeneralTransition.guard gtrans |> Constraint.mk |> RealConstraint.of_intconstraint in
  let constraints_and_atoms =
    match constraint_type with
    | `Non_Increasing ->  RealParameterAtom.Infix.(template >= expected_poly cache gtrans) |> lift_paraatom

    | `Decreasing ->
        RealParameterAtom.Infix.(template >= (RealParameterPolynomial.add (expected_poly cache gtrans) (RealParameterPolynomial.of_polynomial RealPolynomial.one)))
        |> lift_paraatom

    | `Bounded ->
      (* for every transition the guard needs to imply that after the guard is passed and the update is evaluated the template
       * evaluated at the corresponding location is non-negative *)
      let transition_bound t =
        let upd_constrs, templ_subst = encode_update cache t in
        (RealParameterConstraint.of_realconstraint @@ RealConstraint.mk_and (GeneralTransition.guard gtrans |> RealConstraint.of_intconstraint) upd_constrs,
          RealParameterAtom.Infix.(templ_subst >= RealParameterPolynomial.zero))
      in

      GeneralTransition.transitions gtrans
      |> TransitionSet.to_list
      |> List.map (transition_bound)

    | `Bounded_Refined ->
        (* for every transition the guard needs to imply that all possible successor states have the same sign of the evaluated template *)
        let tlist = GeneralTransition.transitions gtrans |> TransitionSet.to_list in
        let t1_nonneg_implies_t2_nonneg t1 t2: RealParameterConstraint.t * (RealParameterAtom.t) =
          (* Under the condition that transition t1 leads to a state where the template is non-negative then transition t2
           * also leads to a succesor state such that the corresponding template is non-negative*)
          let (upd_constraints_t1,template_subst_t1) = encode_update cache t1 in
          let (upd_constraints_t2,template_subst_t2) = encode_update cache t2 in
          let t1_pos = RealParameterConstraint.mk_ge template_subst_t1 RealParameterPolynomial.zero in
          let t2_pos = RealParameterAtom.mk_ge template_subst_t2 RealParameterPolynomial.zero in
          let extended_guard =
            RealParameterConstraint.of_realconstraint guard
            |> RealParameterConstraint.mk_and (RealParameterConstraint.of_realconstraint upd_constraints_t1)
            |> RealParameterConstraint.mk_and (RealParameterConstraint.of_realconstraint upd_constraints_t2)
            |> RealParameterConstraint.mk_and t1_pos
          in

          (extended_guard, t2_pos)
        in

        List.cartesian_product tlist tlist
        |> List.map (uncurry t1_nonneg_implies_t2_nonneg)
  in
  List.map
    (uncurry RealParameterConstraint.farkas_transform)
    constraints_and_atoms
  |> List.map RealFormula.mk
  |> List.fold_left RealFormula.mk_and RealFormula.mk_true

let non_increasing_constraint cache transition =
  general_transition_constraint cache `Non_Increasing transition

let bounded_constraint cache transition =
  let constr = general_transition_constraint cache `Bounded transition in
  constr

let bounded_refined_constraint cache transition =
  let constr = general_transition_constraint cache `Bounded_Refined transition in
  constr

let decreasing_constraint cache transition =
  let constr = general_transition_constraint cache `Decreasing transition in
  constr


module Solver = SMT.IncrementalZ3Solver

(* Add the corresponding bounding constrained depending whether we perform a refind analysis*)
let add_bounding_constraint ~refined cache solver gt =
  (* Try to add the normal bound constraint *)
  if refined then
    Solver.add_real solver (bounded_refined_constraint cache gt)
  else
    Solver.add_real solver (bounded_constraint cache gt)

(* Backtrack the set of non_increasing general transitions.SMT
 * The Tuple's first element is the list of not non-increasing transitions, the second
 * the number of non-increasing transitions, and the third is the list of non-increasing transitions *)
let rec backtrack_1d_non_increasing ~refined cache = function
  | ([],n,ys,solver) -> (n,ys)
  | (x::xs,n,ys,solver) ->
          Solver.push solver;
          Solver.add_real solver (non_increasing_constraint cache x);
          add_bounding_constraint ~refined:refined cache solver x;
          if Solver.satisfiable_option solver = Some true then (
            (* How many non-increasing transitions when keeping the candidate? *)
            let (n2, ys2) = backtrack_1d_non_increasing ~refined:refined cache (xs, n+1, (GeneralTransitionSet.add x ys), solver) in
            if n2-n == List.length (x::xs) then
              (* all remaining transitions are shown to be non-increasing -> we are done *)
              (Solver.pop solver; (n2, ys2))
            else(
              Solver.pop solver;
              (* How many non-increasing transitions when skipping the current candidate? *)
              let (n1, ys1) = backtrack_1d_non_increasing ~refined:refined cache (xs, n, ys, solver) in
              if n1 >= n2 then
                (* When ignoring the current candidate we yield better results. Hence remove the corresponding constraints from the solver*)
                (n1, ys1)
              else
                (* Keep the current candidate and its constraints *)
                (n2, ys2)
            )
          ) else (
            (* The last added constraint renders all constraints unsat. Hence we backtrack *)
            Solver.pop solver;
            backtrack_1d_non_increasing ~refined:refined cache (xs,n,ys,solver)
          )

(* Compute a RSM as needed by KoAT. Optionally we can specify a timeout for the SMT Solver *)
let find_1d_lexrsm_non_increasing ~refined ~timeout cache transitions decreasing =
  let solver = Solver.create ~timeout:timeout ()
  in
  Solver.add_real solver (decreasing_constraint cache decreasing);
  (* Here the non-refined bounded constraint is needed since otherwise the ranking function could become negative after the evaluation of
   * a decreasing transition *)
  add_bounding_constraint ~refined:false cache solver decreasing;
  if Solver.satisfiable_option solver = Some true then
    let (n, non_incr) =
      Logger.log logger Logger.DEBUG (fun () -> "Backtrack non-increasing set", ["decr", GeneralTransition.to_id_string decreasing]);
      backtrack_1d_non_increasing ~refined:refined
        cache (GeneralTransitionSet.remove decreasing transitions |> GeneralTransitionSet.to_list,
               0, GeneralTransitionSet.empty, solver)
      (* Readd constraints *)
      |> tap (GeneralTransitionSet.iter (fun gt -> Solver.add_real solver (non_increasing_constraint cache gt); add_bounding_constraint ~refined:refined cache solver gt) % snd)
    in

    (* minimize the ranking function if possible (ATM Z3 does *not* support non-linear optimization as occuring in the refined case)*)
    if not refined then (
      let fresh_coeffs =
        !fresh_coeffs
        |> List.filter (fun c -> not @@ List.mem c !fresh_consts)
        |> List.map (fun c -> c,OurFloat.of_int 2)
      in
      let fresh_constants = List.map (fun c -> c, OurFloat.one) !fresh_consts in

      Solver.minimize_set_vars solver ~add_as_constraint:true @@ List.map Tuple2.first fresh_coeffs;
      Solver.minimize_absolute solver @@ List.map Tuple2.first fresh_coeffs;
      Solver.minimize_absolute solver @@ List.map Tuple2.first fresh_constants
    );

    Solver.model_real ~optimized:(not refined) solver
    |> fun eval -> (eval, GeneralTransitionSet.add decreasing non_incr)
  else
    (None, GeneralTransitionSet.empty)


let pprf_to_string t =
  "{decreasing: " ^ GeneralTransition.to_id_string t.decreasing ^
  " non_incr: " ^ GeneralTransitionSet.to_id_string t.non_increasing ^
  " rank: [" ^ (
                 LocationSet.union (GeneralTransitionSet.start_locations t.non_increasing) (GeneralTransitionSet.target_locations t.non_increasing)
                 |> LocationSet.to_list
                 |> List.map (fun loc -> Location.to_string loc ^ ": " ^ (t.rank loc |> RealPolynomial.to_string)) |> String.concat "; "
               )
  ^ "]}"

let ranking_table_to_string rtable =
  RankingTable.to_list rtable
  |> List.map (Option.default "None" % Option.map pprf_to_string % Tuple2.second)
  |> String.concat "; "

(* Compute a ranking function as a side effect such that the given transition is decreasing.
 * The computed ranking function is subsequently added to the RankingTable cache *)
let compute_ranking_function ~refined ~timeout cache program gt =
  let gts = Program.generalized_transitions program in

  (* Limit possible non-increasing transitions to those in the same SCC as the given transition *)
  let scc_transitions = Program.sccs program in
  scc_transitions
  |> Enum.filter (not % TransitionSet.is_empty % TransitionSet.inter (GeneralTransition.transitions gt))
  |> Enum.fold TransitionSet.union TransitionSet.empty
  (* Complete the previous transitionset, such that every gt containing a t of the previous set is contained in the resulting gt set *)
  |> fun tset -> TransitionSet.fold
      (fun t -> GeneralTransitionSet.union (GeneralTransitionSet.filter (TransitionSet.mem t % GeneralTransition.transitions) gts))
      tset GeneralTransitionSet.empty
  |> fun gtset -> ignore
      (
        let (eval,non_incr) = find_1d_lexrsm_non_increasing ~refined:refined ~timeout:timeout cache gtset gt in
        let rankfunc = match eval with
          | None -> None
          | (Some valuation) -> Some (fun loc ->
                TemplateTable.find (get_template_table cache) loc
                |> RealParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurFloat.zero) )
        in
        let ranking rfunc = {decreasing = gt; non_increasing = non_incr; rank = rfunc; } in
        RankingTable.add (get_ranking_table cache) gt (Option.map ranking rankfunc)
      );
  Logger.log logger Logger.DEBUG
                  (fun () -> "compute_ranking_function", ["ranking_table", ranking_table_to_string (get_ranking_table cache)])

let print_pprf_option = function
  | Some a -> pprf_to_string a
  | None   -> "None"

(* Coompute a ranking function such that the given gt is decreasing and return it.non_increasing
 * Once a ranking function is computed it is stored in the given cache *)
let find ?(refined=false) ?(timeout=None) cache program gt =
  let execute () =
    let vars = Program.input_vars program in
    let locations = (Program.graph program |> TransitionGraph.locations |> LocationSet.to_list) in
    if TemplateTable.is_empty (get_template_table cache) then
      compute_ranking_templates cache vars locations;
    if Option.is_none (RankingTable.find_option (get_ranking_table cache) gt) then
      compute_ranking_function ~refined:refined ~timeout:timeout cache program gt;
    RankingTable.find (get_ranking_table cache) gt
  in
  Logger.with_log logger Logger.DEBUG
    (fun () -> "LexRSM.find", ["gt", GeneralTransition.to_id_string gt; "refined", Bool.to_string refined])
    ~result:(print_pprf_option) execute

