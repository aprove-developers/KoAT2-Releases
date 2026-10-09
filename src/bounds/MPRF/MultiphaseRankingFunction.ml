open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
module Solver = SMT.IncrementalZ3Solver
module Valuation = Valuation.Make (OurInt)

type measure = [ `Cost | `Time ] [@@deriving show]
type constraint_type = [ `Non_Increasing | `Decreasing ]

let logger = Logging.(get MPRF)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM

  type mprf = (Location.t -> Polynomial.t) list
  type t = { rank : mprf; decreasing : Transition.t; non_increasing : TransitionSet.t; depth : int }

  module TemplateTable = Hashtbl.Make (Location)

  module CoeffsTable = Hashtbl.Make (struct
    type t = Location.t * Var.t

    let equal (l1, v1) (l2, v2) = Location.equal l1 l2 && Var.equal v1 v2
    let hash = Hashtbl.hash
  end)

  type mprf_problem = {
    program : PM.Program.t;
    measure : measure;
    make_non_increasing : TransitionSet.t;
    make_decreasing : Transition.t;
    unbounded_vars : Transition.t -> VarSet.t;
    find_depth : int;
    is_time_bounded : Transition.t -> bool;
  }

  (* Only depends on the program, measure, and depth. *)
  type ranking_cache = {
    template_table : ParameterPolynomial.t TemplateTable.t Array.t;
    coeffs_table : VarSet.t CoeffsTable.t;
    constraint_cache : (int * constraint_type * int, Formula.t) Hashtbl.t;
  }

  (* Cache does not depend on measure since the cache is unique for each measure *)
  let constraint_cache cache =
    Util.memoize cache.constraint_cache ~extractor:(fun (depth, _, constraint_type, t) ->
        (depth, constraint_type, Transition.id t))


  let decreaser measure cost =
    match measure with
    | `Cost -> cost
    | `Time -> Polynomial.one


  (* method transforms polynome to parapolynom*)
  let as_parapoly update var =
    match Base.Map.find update var with
    (* Correct? In the nondeterministic case we just make it deterministic? *)
    | None -> ParameterPolynomial.of_var var
    | Some p -> ParameterPolynomial.of_polynomial p


  (** Given a list of variables an affine template-polynomial is generated*)
  let ranking_template cache location (vars : VarSet.t) : ParameterPolynomial.t * Var.t list =
    let vars = Base.Set.elements vars in
    let num_vars = List.length vars in
    let fresh_vars = Var.fresh_id_list Var.Int num_vars in
    let fresh_coeffs = List.map Polynomial.of_var fresh_vars in

    (if Option.is_some cache && Option.is_some location then
       (* store fresh_vars *)
       let cache = Option.get cache and location = Option.get location in
       let coeff_table = cache.coeffs_table in
       List.iter
         (* (fun (v,v') -> CoeffTable.add coeff_table (location,v) v') *)
         (fun (v, v') ->
           CoeffsTable.modify_def VarSet.empty (location, v)
             (Base.Set.union (VarSet.singleton v'))
             coeff_table)
         (List.combine vars fresh_vars));

    let linear_poly = ParameterPolynomial.of_coeff_list fresh_coeffs vars in
    let constant_var = Var.fresh_id Var.Int () in
    let constant_poly = ParameterPolynomial.of_constant (Polynomial.of_var constant_var) in
    (ParameterPolynomial.(linear_poly + constant_poly), List.append fresh_vars [ constant_var ])


  let rank f = f.rank
  let decreasing f = f.decreasing
  let non_increasing f = f.non_increasing
  let depth f = f.depth

  (* output methods *)
  let rank_to_string (locations : Location.t list)
      (content_to_string : (Location.t -> 'a) list * Location.t -> string) (rank : (Location.t -> 'a) list) =
    locations |> List.enum
    |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (rank, l))


  let polyList_to_string ?(pretty = false) ((rank : (Location.t -> 'a) list), (l : Location.t)) =
    rank |> List.enum
    |> Util.enum_to_string (fun p ->
        (if pretty then
           Polynomial.to_string_pretty
         else
           Polynomial.to_string)
          (p l))


  let only_rank_to_string { rank; decreasing; non_increasing; depth } =
    let locations = non_increasing |> TransitionSet.locations |> Base.Set.to_list in
    rank_to_string locations polyList_to_string rank


  let to_string { rank; decreasing; non_increasing; depth } =
    "{multirank:"
    ^ only_rank_to_string { rank; decreasing; non_increasing; depth }
    ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"


  let compute_proof { rank; decreasing; non_increasing; depth } bound program format =
    let module GraphPrint = GraphPrint.MakeForClassicalAnalysis (PM) in
    let color_map =
      Base.Set.fold
        ~f:(fun colourmap t -> OurBase.Map.set ~key:t ~data:GraphPrint.Blue colourmap)
        non_increasing ~init:GraphPrint.empty_color_map
      |> OurBase.Map.set ~key:decreasing ~data:GraphPrint.Red
    in
    let locations = non_increasing |> TransitionSet.locations |> Base.Set.to_list in
    FormattedString.(
      mk_header_small
        (mk_str
           ("MPRF for transition "
           ^ Transition.to_string_pretty decreasing
           ^ " of depth " ^ string_of_int depth ^ ":"))
      <> mk_paragraph
           ((match bound with
              | Some b ->
                  mk_str "new bound:" <> mk_newline <> mk_paragraph (mk_str (Bound.to_string ~pretty:true b))
              | _ -> FormattedString.Empty)
           <> mk_str "MPRF:" <> mk_newline
           <> (locations
              |> List.map (fun l ->
                  "• " ^ Location.to_string l ^ ": " ^ polyList_to_string ~pretty:true (rank, l))
              |> List.map mk_str_line |> mappend |> mk_paragraph))
      <>
      match format with
      | Formatter.Html -> FormattedString.mk_raw_str (GraphPrint.print_system_pretty_html ~color_map program)
      | _ -> FormattedString.Empty)


  let add_to_proof mprf bound program =
    ProofOutput.add_to_proof_with_format (compute_proof mprf bound program)


  module UnliftedBound = UnliftedBounds.UnliftedTimeBound.Make (PM) (Bound)

  let to_unlifted_bound program t =
    let evaluated_rank_for_entry_loc entry_loc =
      if t.depth = 1 then
        Bound.of_intpoly @@ (List.hd t.rank) entry_loc
      else
        let mprf_coeff = MPRF_Coefficient.coefficient t.depth in
        let ranking_functions_at_entry_loc =
          OurBase.Sequence.of_list t.rank |> OurBase.Sequence.map ~f:(fun f -> f entry_loc)
        in
        Bound.of_intpoly
        @@ Polynomial.(one + (of_constant mprf_coeff * Polynomial.sum ranking_functions_at_entry_loc))
    in
    let locs = OurBase.Set.map (module Location) t.non_increasing ~f:Tuple3.first in
    UnliftedBound.mk_from_program_fcs logger ~handled_transitions:t.non_increasing
      ~measure_decr_transitions:(TransitionSet.singleton t.decreasing)
      ~compute_proof:
        (Option.some @@ fun ~get_timebound ~get_sizebound _ bound -> compute_proof t (Some bound) program)
      program
      (fun (_, _, l') -> evaluated_rank_for_entry_loc l')
      (fun t' ->
        ( OurBase.Set.fold
            ~f:(fun b v -> Bound.add (evaluated_rank_for_entry_loc (VarFunctionCall.return_loc v)) b)
            ~init:Bound.zero (Transition.function_call_vars t'),
          OurBase.Set.filter
            ~f:(fun v -> OurBase.Set.mem locs (VarFunctionCall.return_loc v))
            (Transition.function_call_vars t') ))


  (* We do not minimise the coefficients for now *)
  (* let fresh_coeffs: Var.t list ref = ref [] *)

  let compute_ranking_templates_ (depth : int) (vars : VarSet.t) (locations : Location.t list)
      ranking_template_ template_table_ to_string : unit =
    let execute (i : int) =
      let ins_loc_prf location =
        (* Each location needs its own ranking template with different fresh variables *)
        let parameter_poly, fresh_vars = ranking_template_ location vars in
        (location, parameter_poly, fresh_vars)
      in
      let templates = List.map ins_loc_prf locations in
      templates
      |> List.iter (fun (location, polynomial, _) ->
          TemplateTable.add (Array.get template_table_ i) location polynomial)
      (*
      * if store_coeffs then templates
      |> List.map (fun (_,_,fresh_vars) -> fresh_vars)
      |> List.flatten
      |> (fun fresh_vars -> fresh_coeffs := fresh_vars) *)
    in
    for i = 0 to depth - 1 do
      Logger.with_log logger Logger.DEBUG
        (fun () -> ("compute_mprf_templates_" ^ string_of_int i, []))
        ~result:(fun () ->
          Array.get template_table_ i |> TemplateTable.enum
          |> Util.enum_to_string (fun (location, polynomial) ->
              Location.to_string location ^ ": " ^ to_string polynomial))
        (fun () -> execute i)
    done


  let compute_ranking_templates cache (depth : int) (vars : VarSet.t) (locations : Location.t list) : unit =
    compute_ranking_templates_ depth vars locations
      (ranking_template (Option.some cache) % Option.some)
      cache.template_table ParameterPolynomial.to_string


  let new_cache program max_depth =
    let new_template_table () = Array.init max_depth (fun i -> TemplateTable.create 10) in
    let cache =
      {
        template_table = new_template_table ();
        coeffs_table = CoeffsTable.create 10;
        constraint_cache = Hashtbl.create 10;
      }
    in
    let locations = OurBase.Set.to_list @@ PM.TransitionGraph.locations (PM.Program.graph program) in
    compute_ranking_templates cache max_depth (PM.Program.input_vars program) locations;
    cache


  let apply_farkas pre concl =
    ParameterConstraint.(farkas_transform (of_constraint @@ Constraint.drop_nonlinear pre)) concl


  (* Methods define properties of mprf *)

  (* method for mprf and functions f_2 to f_d of depth i *)
  let transition_constraint_i_ (measure, constraint_type) (update, guard, cost) template0_l template1_l
      template1_l' : Formula.t =
    let poly = ParameterPolynomial.add template0_l template1_l in
    let atom =
      match constraint_type with
      | `Non_Increasing ->
          ParameterAtom.Infix.(
            template1_l >= ParameterPolynomial.substitute_f (as_parapoly update) template1_l')
      | `Decreasing ->
          ParameterAtom.Infix.(
            poly
            >= ParameterPolynomial.(
                 ParameterPolynomial.of_polynomial (decreaser measure cost)
                 + substitute_f (as_parapoly update) template1_l'))
    in
    apply_farkas guard atom |> Formula.mk


  let transition_constraint_i (template_table0, template_table1, measure, constraint_type, (l, t, l')) :
      Formula.t =
    let template0_l = TemplateTable.find template_table0 l
    and template1_l = TemplateTable.find template_table1 l
    and template1_l' = TemplateTable.find template_table1 l' in
    transition_constraint_i_ (measure, constraint_type)
      TransitionLabel.TransitionLabelNonFunctionCall.(update_map t, guard t, cost t)
      template0_l template1_l template1_l'


  (* method for mprf and function f_1*)
  let transition_constraint_1_ (measure, constraint_type) (update, guard, cost) template1_l template1_l' :
      Formula.t =
    let atom =
      match constraint_type with
      | `Non_Increasing ->
          ParameterAtom.Infix.(
            template1_l >= ParameterPolynomial.substitute_f (as_parapoly update) template1_l')
      | `Decreasing ->
          ParameterAtom.Infix.(
            template1_l
            >= ParameterPolynomial.(
                 ParameterPolynomial.of_polynomial (decreaser measure cost)
                 + substitute_f (as_parapoly update) template1_l'))
    in
    apply_farkas guard atom |> Formula.mk


  let transition_constraint_1 (template_table1, measure, constraint_type, (l, t, l')) : Formula.t =
    let template1_l = TemplateTable.find template_table1 l
    and template1_l' = TemplateTable.find template_table1 l' in
    transition_constraint_1_ (measure, constraint_type)
      TransitionLabel.TransitionLabelNonFunctionCall.(update_map t, guard t, cost t)
      template1_l template1_l'


  (* method for mprf and function f_d*)
  let transition_constraint_d_ bound (measure, constraint_type) guard template1_l : Formula.t =
    match constraint_type with
    | `Non_Increasing -> Formula.mk_true
    | `Decreasing ->
        let atom = ParameterAtom.Infix.(template1_l >= bound) in
        apply_farkas guard atom |> Formula.mk


  let transition_constraint_d bound (template_table1, measure, constraint_type, (l, t, l')) : Formula.t =
    let template1_l = TemplateTable.find template_table1 l in
    transition_constraint_d_ bound (measure, constraint_type)
      (TransitionLabel.TransitionLabelNonFunctionCall.guard t)
      template1_l


  (* use all three functions above combined*)
  let transition_constraint_ cache (depth, measure, constraint_type, (l, (t : TransitionLabel.t), l')) :
      Formula.t =
    let t_non_rec = TransitionLabel.overapprox_function_calls t in
    let res = ref Formula.mk_true in
    for i = 1 to depth - 1 do
      res :=
        ( Array.get cache.template_table (i - 1),
          Array.get cache.template_table i,
          measure,
          constraint_type,
          (l, t_non_rec, l') )
        |> transition_constraint_i |> Formula.mk_and !res
    done;
    res :=
      (Array.get cache.template_table 0, measure, constraint_type, (l, t_non_rec, l'))
      |> transition_constraint_1 |> Formula.mk_and !res;
    if depth > 1 then
      res :=
        (Array.get cache.template_table (depth - 1), measure, constraint_type, (l, t_non_rec, l'))
        |> transition_constraint_d ParameterPolynomial.zero
        |> Formula.mk_and !res
    else
      res :=
        (Array.get cache.template_table (depth - 1), measure, constraint_type, (l, t_non_rec, l'))
        |> transition_constraint_d ParameterPolynomial.one
        |> Formula.mk_and !res;

    !res


  let transition_constraint cache = constraint_cache cache (transition_constraint_ cache)

  let non_increasing_constraint cache depth measure transition =
    transition_constraint cache (depth, measure, `Non_Increasing, transition)


  let decreasing_constraint cache depth measure transition =
    transition_constraint cache (depth, measure, `Decreasing, transition)


  (** A valuation is a function which maps from a finite set of variables to values *)
  let rank_from_valuation_ valuation =
    ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)


  let rank_from_valuation cache depth (i : int) valuation location =
    rank_from_valuation_ valuation (TemplateTable.find (Array.get cache.template_table i) location)


  let make cache depth decreasing_transition non_increasing_transitions valuation =
    {
      rank = List.init depth (fun i -> rank_from_valuation cache depth i valuation);
      decreasing = decreasing_transition;
      non_increasing = non_increasing_transitions;
      depth;
    }


  type formula_conjunction = Formula.t List.t
  (*  formulas are in DNF. For conjunctions of formulas, avoid costly reconversion to DNF *)

  (** Searches for a set of non-increasing transitions and an MPRF with a single SMT query.
      Every candidate transition [t] gets an indicator variable in \{0,1\}, which is 1 iff [t] is non-increasing.
      A non-increasing set is admissible if all its entry transitions are time-bounded, and the coefficients of the
      variables which are unbounded on an entry transition are 0 at its target location. *)
  let compute_scc cache problem =
    let non_increasing_indicators =
      OurBase.Set.to_list problem.make_non_increasing
      |> List.map (fun t -> (t, Var.fresh_id Var.Int ()))
      |> OurBase.Map.of_alist_exn (module Transition)
    in
    let in_non_increasing_set t =
      OurBase.Map.find non_increasing_indicators t
      |> Option.map_default
           (fun indicator -> Formula.mk_eq (Polynomial.of_var indicator) Polynomial.one)
           Formula.mk_false
    and not_in_non_increasing_set t =
      OurBase.Map.find non_increasing_indicators t
      |> Option.map_default
           (fun indicator -> Formula.mk_eq (Polynomial.of_var indicator) Polynomial.zero)
           Formula.mk_true
    in
    let indicator_bounds indicator : formula_conjunction =
      Polynomial.[ Formula.mk_ge (of_var indicator) zero; Formula.mk_le (of_var indicator) one ]
    in
    let transition_constraints t : formula_conjunction =
      if Transition.equal t problem.make_decreasing then
        [ in_non_increasing_set t; decreasing_constraint cache problem.find_depth problem.measure t ]
      else
        let decreasing =
          if problem.find_depth = 1 then
            []
          else
            [ decreasing_constraint cache problem.find_depth problem.measure t ]
        in
        (* Note that decreasing constraint is *not* subsumed by the non-increasing constraint.
           Hence, the decreasing constraint is allowed instead, unless the depth is 1, where it implies the
           non-increasing constraint. *)
        [
          Formula.any
            (not_in_non_increasing_set t
            :: non_increasing_constraint cache problem.find_depth problem.measure t
            :: decreasing);
        ]
    in
    let zero_coefficients_at_target entry_transition =
      OurBase.Set.to_list (problem.unbounded_vars entry_transition)
      |> List.concat_map (fun v ->
          OurBase.Set.to_list @@ CoeffsTable.find cache.coeffs_table (Transition.target entry_transition, v))
      |> List.map (fun coeff -> Formula.mk_eq (Polynomial.of_var coeff) Polynomial.zero)
      |> Formula.all
    in
    let entry_constraints t =
      OurBase.Set.to_list (Program.pre problem.program t)
      |> List.map (fun pre ->
          if problem.is_time_bounded pre then
            Formula.any
              (* This reads: t is entry transition → all vars occuring in template (with non-zero coeff) are bounded *)
              [ not_in_non_increasing_set t; in_non_increasing_set pre; zero_coefficients_at_target pre ]
          else
            Formula.mk_or (not_in_non_increasing_set t) (in_non_increasing_set pre))
    in
    (* The constraints are added separately, as their conjunction would be expanded into a large DNF *)
    let solver = Solver.create () in
    List.concat_map indicator_bounds (OurBase.Map.data non_increasing_indicators)
    @ List.concat_map
        (fun t -> transition_constraints t @ entry_constraints t)
        (OurBase.Set.to_list problem.make_non_increasing)
    |> List.iter (Solver.add solver);
    let ranking_function =
      Solver.model solver
      |> Option.map (fun valuation ->
          let non_increasing =
            OurBase.Set.filter problem.make_non_increasing ~f:(fun t ->
                OurInt.equal OurInt.one
                  (Valuation.eval_opt (OurBase.Map.find_exn non_increasing_indicators t) valuation
                  |? OurInt.zero))
          in
          make cache problem.find_depth problem.make_decreasing non_increasing valuation)
    in
    (match ranking_function with
    | Some ranking_function ->
        Logger.(
          log logger INFO (fun () ->
              ( "add_mprf",
                [
                  ("measure", show_measure problem.measure);
                  ("decreasing", Transition.to_id_string problem.make_decreasing);
                  ("non_increasing", TransitionSet.to_id_string ranking_function.non_increasing);
                  ("rank", only_rank_to_string ranking_function);
                ] )))
    | None ->
        Logger.(
          log logger WARN (fun () ->
              ( "no_mprf",
                [
                  ("measure", show_measure problem.measure);
                  ("transition", Transition.to_id_string problem.make_decreasing);
                ] ))));
    ranking_function


  let find_scc_with_cache cache measure program ~is_time_bounded ~unbounded_vars (scc : TransitionSet.t) depth
      make_decreasing =
    let locs = LocationSet.map ~f:Transition.src (OurBase.Set.add scc make_decreasing) in
    let transitions_without_looping_fc =
      OurBase.Set.filter
        ~f:(fun t ->
          not
          @@ OurBase.Set.exists
               ~f:(fun v -> OurBase.Set.mem locs (VarFunctionCall.return_loc v))
               (Transition.function_call_vars t))
        scc
    in
    if OurBase.Set.is_empty scc || (not @@ OurBase.Set.mem transitions_without_looping_fc make_decreasing)
    then
      None
    else
      let mprf_problem =
        {
          program;
          measure;
          make_non_increasing =
            (if OurBase.Set.is_empty (OurBase.Set.diff scc transitions_without_looping_fc) then
               scc
             else
               TransitionSet.singleton make_decreasing);
          make_decreasing;
          unbounded_vars;
          find_depth = depth;
          is_time_bounded;
        }
      in
      let execute () = compute_scc cache mprf_problem in
      Logger.with_log logger Logger.DEBUG
        (fun () ->
          ("find_scc", [ ("measure", show_measure measure); ("scc", TransitionSet.to_id_string scc) ]))
        ~result:(Util.enum_to_string to_string % Option.enum)
        execute


  let find_scc measure program ~is_time_bounded ~unbounded_vars scc depth make_decreasing =
    find_scc_with_cache (new_cache program depth) measure program ~is_time_bounded ~unbounded_vars scc depth
      make_decreasing


  let find_scc_batch measure program ~is_time_bounded ~unbounded_vars depth sccs_and_decreasing =
    let cache = new_cache program depth in
    List.map
      (fun (scc, make_decreasing) ->
        find_scc_with_cache cache measure program ~is_time_bounded ~unbounded_vars scc depth make_decreasing)
      sccs_and_decreasing


  let find measure program depth =
    let execute () =
      Base.Sequence.of_list (PM.Program.sccs program)
      |> Base.Sequence.map ~f:(fun scc ->
          Base.Set.to_sequence scc
          |> Base.Sequence.map
               ~f:
                 (find_scc measure program ~is_time_bounded:(const false) ~unbounded_vars:(const VarSet.empty)
                    scc depth))
      |> Base.Sequence.join |> Base.Sequence.filter_opt
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("find", [ ("measure", show_measure measure) ]))
      ~result:(Util.sequence_to_string ~f:to_string)
      execute


  module Loop = Loop.Make (Bound) (PM.TransitionLabel.TransitionLabelNonFunctionCall)

  type t_loop = { rank : Polynomial.t list; depth : int }

  (* Computes a MPRF for a loop (with cost 1). *)
  let find_for_loop loop ?(non_increasing = None) depth =
    let template_table = Array.init depth (fun _ -> ranking_template None None (Loop.vars loop)) in
    let template_i i = Tuple2.first @@ Array.get template_table (i - 1) in
    let res = ref Formula.mk_true in
    let constraint_of_guard measurement loop guard =
      for i = 1 to depth - 1 do
        res :=
          transition_constraint_i_ (`Time, measurement)
            (Loop.update loop, guard, Polynomial.one)
            (template_i i)
            (template_i (i + 1))
            (template_i (i + 1))
          |> Formula.mk_and !res
      done;
      res :=
        transition_constraint_1_ (`Time, measurement)
          (Loop.update loop, guard, Polynomial.one)
          (template_i 1) (template_i 1)
        |> Formula.mk_and !res;
      if depth > 1 then
        res :=
          transition_constraint_d_ ParameterPolynomial.zero (`Time, measurement) guard (template_i depth)
          |> Formula.mk_and !res
      else
        res :=
          transition_constraint_d_ ParameterPolynomial.one (`Time, measurement) guard (template_i depth)
          |> Formula.mk_and !res
    in
    List.iter (constraint_of_guard `Decreasing loop) (Loop.guard loop |> Formula.constraints);
    if Option.is_some non_increasing then
      List.iter
        (fun loop ->
          List.iter (constraint_of_guard `Non_Increasing loop) (Loop.guard loop |> Formula.constraints))
        (Option.get non_increasing);
    let model = SMT.Z3Solver.get_model !res in
    if Option.is_some model then
      Option.some
        {
          rank = List.init depth (fun i -> rank_from_valuation_ (Option.get model) (template_i (i + 1)));
          depth;
        }
    else
      None


  let time_bound loop ?(non_increasing = None) max_depth =
    let rankfuncs =
      Enum.seq 1 (( + ) 1) (( > ) (max_depth + 1))
      |> Enum.map (find_for_loop loop ~non_increasing)
      |> Enum.peek % Enum.filter Option.is_some
      |? None
    in
    if Option.is_some rankfuncs then
      let rankfuncs = Option.get rankfuncs in
      let bounds = rankfuncs.rank |> List.map Bound.of_intpoly in
      Bound.(one + (of_OurInt (MPRF_Coefficient.coefficient rankfuncs.depth) * Bound.sum_list bounds))
    else
      Bound.infinity
end

include Make (Bounds.Bound) (ProgramModules)
