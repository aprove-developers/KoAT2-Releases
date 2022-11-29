open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open BoundsInst

module Solver = SMT.IncrementalZ3Solver
module Valuation = Valuation.Make(OurInt)

type measure = [ `Cost | `Time ] [@@deriving show]

type constraint_type = [`Non_Increasing | `Decreasing]

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  type mprf = (Location.t -> Polynomial.t) list

  type t = {
    rank : mprf;
    decreasing : Transition.t;
    non_increasing : TransitionSet.t;
    depth : int;
  }
  module TemplateTable = Hashtbl.Make(Location)

  module CoeffsTable = Hashtbl.Make(struct
                                    type t = Location.t * Var.t
                                    let equal (l1,v1) (l2,v2) = Location.equal l1 l2 && Var.equal v1 v2
                                    let hash = Hashtbl.hash
                                    end)

  type mprf_problem = {
    program: Program.t;
    measure: measure;
    make_non_increasing: Transition.t Array.t;
    make_decreasing: Transition.t;
    unbounded_vars: Transition.t -> VarSet.t;
    find_depth: int;
    is_time_bounded: Transition.t -> bool;
  }

  type ranking_cache = {
    rank_func: t option ref;
    template_table: ParameterPolynomial.t TemplateTable.t Array.t;
    coeffs_table: VarSet.t CoeffsTable.t;
    constraint_cache: (int * constraint_type * int, Formula.t) Hashtbl.t;
  }

  let new_cache max_depth =
    let new_template_table () = Array.init max_depth (fun i -> TemplateTable.create 10) in
    {
      rank_func = ref None;
      template_table = new_template_table ();
      coeffs_table = CoeffsTable.create 10;
      constraint_cache = Hashtbl.create 10;
    }

  (* Cache does not depend on measure since the cache is unique for each measure *)
  let constraint_cache cache =
    Util.memoize cache.constraint_cache ~extractor:(fun (depth, _, constraint_type, t) -> depth, constraint_type, Transition.id t)

  let logger = Logging.(get PRF)

  let decreaser measure t =
      match measure with
      | `Cost -> TransitionLabel.cost t
      | `Time -> Polynomial.one

  (* method transforms polynome to parapolynom*)
  let as_parapoly label var =
      match TransitionLabel.update label var with
      (** Correct? In the nondeterministic case we just make it deterministic? *)
      | None -> ParameterPolynomial.of_var var
      | Some p -> ParameterPolynomial.of_polynomial p

  (** Given a list of variables an affine template-polynomial is generated*)
  let ranking_template cache location (vars: VarSet.t): ParameterPolynomial.t * Var.t list =
      let vars = VarSet.elements vars in
      let num_vars = List.length vars in
      let fresh_vars = Var.fresh_id_list Var.Int num_vars in
      let fresh_coeffs = List.map Polynomial.of_var fresh_vars in

      (* store fresh_vars *)
      let coeff_table = cache.coeffs_table in
      List.iter
        (* (fun (v,v') -> CoeffTable.add coeff_table (location,v) v') *)
        (fun (v,v') -> CoeffsTable.modify_def VarSet.empty (location,v) (VarSet.union (VarSet.singleton v')) coeff_table)
        (List.combine vars fresh_vars);

      let linear_poly = ParameterPolynomial.of_coeff_list fresh_coeffs vars in
      let constant_var = Var.fresh_id Var.Int () in
      let constant_poly = ParameterPolynomial.of_constant (Polynomial.of_var constant_var) in
      ParameterPolynomial.(linear_poly + constant_poly),
      List.append fresh_vars [constant_var]

  let rank f = f.rank

  let decreasing f = f.decreasing

  let non_increasing f = f.non_increasing

  let depth f = f.depth

  (* output methods *)
  let rank_to_string (locations: Location.t list) (content_to_string: ((Location.t -> 'a) list) * Location.t -> string) (rank: (Location.t -> 'a) list) =
    locations
    |> List.enum
    |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (rank ,l))

  let polyList_to_string ?(pretty=false) ((rank: (Location.t -> 'a) list) , (l : Location.t)) =
    rank
    |> List.enum
    |> Util.enum_to_string (fun p -> (if pretty then Polynomial.to_string_pretty else Polynomial.to_string) (p l))

  let only_rank_to_string {rank; decreasing; non_increasing; depth} =
    let locations = non_increasing |> TransitionSet.locations |> LocationSet.to_list in
    rank_to_string locations polyList_to_string rank

  let to_string {rank; decreasing; non_increasing; depth} =
    "{multirank:" ^ only_rank_to_string {rank; decreasing; non_increasing; depth} ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"

  let add_to_proof {rank; decreasing; non_increasing; depth} bound program =
    let module GraphPrint = GraphPrint.Make(PM) in
    let color_map =
      TransitionSet.fold (fun t -> GraphPrint.TransitionMap.add t GraphPrint.Blue) non_increasing GraphPrint.TransitionMap.empty
      |> GraphPrint.TransitionMap.add decreasing GraphPrint.Red
    in
    let locations = non_increasing |> TransitionSet.locations |> LocationSet.to_list in
    ProofOutput.add_to_proof_with_format @@ FormattedString.(fun format ->
      mk_header_small (mk_str ("MPRF for transition " ^ Transition.to_string_pretty decreasing ^ " of depth " ^ string_of_int depth ^ ":")) <>
      mk_paragraph (
        mk_str "new bound:" <> mk_newline <> mk_paragraph (mk_str (Bound.to_string ~pretty:true bound)) <>
        mk_str "MPRF:" <> mk_newline <>
          (locations |> List.map (fun l -> "• " ^ Location.to_string l ^ ": " ^ polyList_to_string ~pretty:true (rank, l)) |> List.map (mk_str_line) |> mappend |> mk_paragraph)) <>
          match format with
          | Html -> FormattedString.mk_raw_str (GraphPrint.print_system_pretty_html color_map program)
          | _    -> FormattedString.Empty
    )

  (* We do not minimise the coefficients for now *)
  (* let fresh_coeffs: Var.t list ref = ref [] *)

  let compute_ranking_templates_ (depth: int) (vars: VarSet.t) (locations: Location.t list) ranking_template_ template_table_ to_string: unit =
    let execute (i:int) =
      let ins_loc_prf location =
        (* Each location needs its own ranking template with different fresh variables *)
        let (parameter_poly, fresh_vars) = ranking_template_ location vars in
        (location, parameter_poly, fresh_vars)
      in
      let templates = List.map ins_loc_prf locations in
      templates
      |> List.iter (fun (location,polynomial,_) -> TemplateTable.add (Array.get template_table_ i) location polynomial);
      (*
      * if store_coeffs then templates
      |> List.map (fun (_,_,fresh_vars) -> fresh_vars)
      |> List.flatten
      |> (fun fresh_vars -> fresh_coeffs := fresh_vars) *)
    in
    for i = 0 to depth - 1 do
      Logger.with_log logger Logger.DEBUG
        (fun () -> "compute_mprf_templates_" ^ string_of_int i, [])
        ~result:(fun () ->
            (Array.get template_table_ i)
            |> TemplateTable.enum
            |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ to_string polynomial)
          )
        (fun () -> execute i);
    done

  let compute_ranking_templates cache (depth: int) (vars: VarSet.t) (locations: Location.t list) : unit =
    compute_ranking_templates_ depth vars locations (ranking_template cache) cache.template_table ParameterPolynomial.to_string

  let apply_farkas pre concl =
    ParameterConstraint.(farkas_transform (of_constraint @@ Constraint.drop_nonlinear pre)) concl

  (* Methods define properties of mprf *)

  (* method for mprf and functions f_2 to f_d of depth i *)
  let transition_constraint_i (template_table0, template_table1, measure, constraint_type, (l,t,l')): Formula.t =
    let template0 = TemplateTable.find template_table0 in
    let template1 = TemplateTable.find template_table1 in
    let poly = ParameterPolynomial.add (template0 l) (template1 l) in
    let atom =
      match constraint_type with
      | `Non_Increasing -> ParameterAtom.Infix.(template1 l  >= ParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
      | `Decreasing -> ParameterAtom.Infix.(poly >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
    in
    apply_farkas (TransitionLabel.guard t) atom
    |> Formula.mk

  (* method for mprf and function f_1*)
  let transition_constraint_1 (template_table1, measure, constraint_type, (l,t,l')): Formula.t =
    let template1 = TemplateTable.find template_table1 in
    let atom =
      match constraint_type with
        | `Non_Increasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
        | `Decreasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
    in
    apply_farkas (TransitionLabel.guard t) atom
    |> Formula.mk

  (* method for mprf and function f_d*)
  let transition_constraint_d bound (template_table1, measure, constraint_type, (l,t,l')) : Formula.t =
    let template1 = TemplateTable.find template_table1 in
      match constraint_type with
      | `Non_Increasing -> Formula.mk_true
      | `Decreasing  -> (
        let atom = ParameterAtom.Infix.(template1 l  >= bound) in
          apply_farkas (TransitionLabel.guard t) atom
          |> Formula.mk)

  (* use all three functions above combined*)
  let transition_constraint_ cache (depth, measure, constraint_type, (l,t,l')): Formula.t =
    let res = ref Formula.mk_true in
      for i = 1 to (depth - 1) do
        res := (Array.get cache.template_table (i - 1), Array.get cache.template_table i, measure, constraint_type, (l,t,l'))
              |> transition_constraint_i
              |> Formula.mk_and !res
      done;
      res := ((Array.get cache.template_table 0), measure, constraint_type, (l,t,l'))
            |> transition_constraint_1
            |> Formula.mk_and !res;
      if depth > 1 then
        res := ((Array.get cache.template_table (depth - 1)), measure, constraint_type, (l,t,l'))
            |> transition_constraint_d ParameterPolynomial.zero
            |> Formula.mk_and !res
      else
        res := ((Array.get cache.template_table (depth - 1)), measure, constraint_type, (l,t,l'))
            |> transition_constraint_d ParameterPolynomial.one
            |> Formula.mk_and !res;

      !res

  let transition_constraint cache = constraint_cache cache (transition_constraint_ cache)

  let non_increasing_constraint cache depth measure transition =
    transition_constraint cache (depth, measure, `Non_Increasing, transition)

  let decreasing_constraint cache depth measure transition =
    transition_constraint cache (depth, measure, `Decreasing, transition)

  (** A valuation is a function which maps from a finite set of variables to values *)

  let rank_from_valuation cache depth (i: int) valuation location =
    location
    |> TemplateTable.find (Array.get cache.template_table i)
    |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)

  let make cache depth decreasing_transition non_increasing_transitions valuation  =
  {
    rank = List.init depth (fun i -> rank_from_valuation cache depth i valuation);
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
    depth = depth;
  }

  let entry_transitions_from_non_increasing program non_increasing =
    let all_possible_pre_trans =
      Stack.enum non_increasing
      |> Enum.fold (fun tset -> TransitionSet.union tset % Program.pre_transitionset_cached program) TransitionSet.empty
    in
    TransitionSet.diff all_possible_pre_trans (TransitionSet.of_enum @@ Stack.enum non_increasing)

  let add_decreasing_constraint cache problem solver_int =
    Solver.add solver_int (decreasing_constraint cache problem.find_depth problem.measure problem.make_decreasing)

  let add_non_increasing_constraint cache problem solver_int transition =
    Solver.push solver_int;
    Solver.add solver_int (non_increasing_constraint cache problem.find_depth problem.measure transition);
    (* Note that decreasing constraint is *not* subsumed by the non-increasing constraint.
      Hence, we first try to add the non-increasing constraint. If this does not work, we try the decreasing constraint instead.
    *)
    if Solver.satisfiable solver_int then (
      Solver.pop solver_int;
      Solver.add solver_int (non_increasing_constraint cache problem.find_depth problem.measure transition);
    ) else (
      Solver.pop solver_int;
      add_decreasing_constraint cache {problem with make_decreasing = transition} solver_int
    )

  let finalise_mprf cache solver_int non_increasing entry_transitions problem =
    (* Set the coefficients for all variables for which a corresponding size bound does not exist for the entry transitions to
    * 0. *)
    let entry_trans_grouped_by_loc =
      List.sort (fun (_,_,l'1) (_,_,l'2) -> Location.compare l'1 l'2)  (TransitionSet.to_list entry_transitions)
      |> List.group_consecutive (fun (_,_,l'1) (_,_,l'2) -> Location.equal l'1 l'2)
    in
    let unbounded_vars_at_entry_locs coeff_table =
      List.map
        (fun ts ->
          let entryloc = Transition.target (List.hd ts) in
          List.enum ts
          |> Enum.map problem.unbounded_vars
          |> Enum.fold VarSet.union VarSet.empty
          |> VarSet.enum
          |> Enum.map (fun v -> VarSet.enum @@ CoeffsTable.find coeff_table (entryloc,v))
          |> Enum.flatten
          |> VarSet.of_enum)
        entry_trans_grouped_by_loc
      |> List.fold_left VarSet.union VarSet.empty
    in

    Solver.push solver_int;
    VarSet.iter (Solver.add solver_int % Formula.mk_eq Polynomial.zero % Polynomial.of_var)
      (unbounded_vars_at_entry_locs cache.coeffs_table);
    if Solver.satisfiable solver_int then (
      (* Solver.minimize_absolute solver_int !fresh_coeffs; *)
      Solver.model solver_int
      |> Option.map (make cache problem.find_depth problem.make_decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum))
      |> Option.may (fun ranking_function ->
          cache.rank_func := Some ranking_function;
          Logger.(log logger INFO (fun () -> "add_mprf", [
                                        "measure", show_measure problem.measure;
                                        "decreasing", Transition.to_id_string problem.make_decreasing;
                                        "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                        "rank", only_rank_to_string ranking_function])));
          raise Exit
    )
    else (
      Solver.pop solver_int;
    )

  let rec backtrack cache (steps_left: int) (index: int) (solver_int: Solver.t) (non_increasing: Transition.t Stack.t) problem =
      let finalise_if_entrytime_bounded non_increasing =
        let entry_trans = entry_transitions_from_non_increasing problem.program non_increasing in
        if TransitionSet.for_all problem.is_time_bounded entry_trans then
          finalise_mprf cache solver_int non_increasing entry_trans problem;
      in

      if Solver.satisfiable solver_int then (
        if steps_left == 0 then (
          finalise_if_entrytime_bounded non_increasing
        ) else (
          for i=index to Array.length problem.make_non_increasing - 1 do
            let transition = Array.get problem.make_non_increasing i in

            Solver.push solver_int;

            add_non_increasing_constraint cache problem solver_int transition;

            Stack.push transition non_increasing;
            backtrack cache (steps_left - 1) (i + 1)  solver_int non_increasing problem;
            ignore (Stack.pop non_increasing);

            Solver.pop solver_int;
          done;
          finalise_if_entrytime_bounded non_increasing
        )
      )

  let get_minimum_applicable_non_inc_set mprf_problem =
    let possible_non_inc_set = TransitionSet.of_array mprf_problem.make_non_increasing in
    let rec helper min_applicable =
      (* get time_unbounded pre transitions *)
      TransitionSet.enum min_applicable
      |> Enum.map (Program.pre_transitionset_cached mprf_problem.program)
      (* necessary since min_applicable can contain all possible pre transitions which may be outside of the current scc*)
      |> Enum.map TransitionSet.enum
      |> Enum.flatten
      |> Enum.filter (not % mprf_problem.is_time_bounded)
      |> TransitionSet.of_enum

      (* add previously found transitions*)
      |> TransitionSet.union min_applicable
      (* we can only consider scc transitions *)
      |> TransitionSet.inter possible_non_inc_set
      |> fun tset -> if TransitionSet.cardinal tset > TransitionSet.cardinal min_applicable then helper tset else tset
    in
    helper (TransitionSet.singleton mprf_problem.make_decreasing)


  let compute_scc cache program mprf_problem =
    let locations = LocationSet.to_list @@ TransitionGraph.locations (Program.graph program) in
    let vars = Program.input_vars program in
    compute_ranking_templates cache mprf_problem.find_depth vars locations;

    let solver_int = Solver.create () in

    (* make transition decreasing*)
    add_decreasing_constraint cache mprf_problem solver_int;

    (* initial constraint propagation for incoming timebounds*)
    let min_applicable = get_minimum_applicable_non_inc_set mprf_problem in
    Logger.log logger Logger.DEBUG
        (fun () -> "compute_scc", [ "decreasing", Transition.to_id_string mprf_problem.make_decreasing
                                  ; "min_applicable_non_inc_set", TransitionSet.to_id_string min_applicable]);
    let non_inc = Stack.of_enum (TransitionSet.enum min_applicable) in
    let make_non_increasing = TransitionSet.to_array @@ TransitionSet.diff (TransitionSet.of_array mprf_problem.make_non_increasing) min_applicable in
    TransitionSet.iter (add_non_increasing_constraint cache mprf_problem solver_int) @@ TransitionSet.remove mprf_problem.make_decreasing min_applicable;
    (try
      backtrack cache
                (Array.length make_non_increasing)
                0
                solver_int
                non_inc
                ({mprf_problem with make_non_increasing;})
    with Exit -> ());

    if Option.is_none !(cache.rank_func) then
      Logger.(log logger WARN (fun () -> "no_mprf", ["measure", show_measure mprf_problem.measure; "transition", Transition.to_id_string mprf_problem.make_decreasing]))

  let find_scc measure program is_time_bounded unbounded_vars scc depth make_decreasing =
    let cache = new_cache depth in
    let mprf_problem =
      { program; measure; make_non_increasing = TransitionSet.to_array scc; make_decreasing; unbounded_vars; find_depth = depth; is_time_bounded}
    in
    let execute () =
      compute_scc cache program mprf_problem;
      !(cache.rank_func)
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> "find_scc", ["measure", show_measure measure; "scc", TransitionSet.to_id_string scc])
      ~result:(Util.enum_to_string to_string % Option.enum)
      execute

  let find  measure program depth =
    let execute () =
      Program.sccs program
      |> Enum.map (fun scc -> Enum.map (find_scc measure program (const false) (const VarSet.empty) scc depth) @@ TransitionSet.enum scc)
      |> Util.cat_maybes_enum % Enum.flatten
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> "find", ["measure", show_measure measure])
      ~result:(Util.enum_to_string to_string)
      execute

end

include Make(ProgramModules)
