open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open CFR

(** Class is derived from RankingFunction.ml*)

module SMTSolver = SMT.IncrementalZ3Solver
module SMTSolverInt = SMT.SolverFast
module Valuation = Valuation.Make(OurInt)

type mprf = (Location.t -> Polynomial.t) list

type t = {
  rank : mprf;
  decreasing : Transition.t;
  non_increasing : TransitionSet.t;
  depth : int;
}

type measure = [ `Cost | `Time ] [@@deriving show, eq]

type constraint_type = [`Non_Increasing | `Decreasing] [@@deriving show, eq]

module RankingTable = Hashtbl.Make(struct include Transition let equal = Transition.same end)

module TemplateTable = Hashtbl.Make(Location)

module CoeffsTable = Hashtbl.Make(struct
                                   type t = Location.t * Var.t
                                   let equal (l1,v1) (l2,v2) = Location.equal l1 l2 && Var.equal v1 v2
                                   let hash = Hashtbl.hash
                                  end)

type mprf_problem = {
  map_trans_pre_trans: TransitionSet.t TransitionTable.t;
  measure: measure;
  scc: Transition.t Array.t;
  make_decreasing: TransitionSet.t;
  unbounded_vars: Transition.t -> VarSet.t;
  find_depth: int;
  is_time_bounded: Transition.t -> bool;
}

type ranking_cache = {
  ranking_table: t RankingTable.t;
  template_table: ParameterPolynomial.t TemplateTable.t list Batteries.ref;
  template_table_real: RealParameterPolynomial.t TemplateTable.t list Batteries.ref;
  coeffs_table: VarSet.t CoeffsTable.t;
  coeffs_table_real: VarSet.t CoeffsTable.t;
  constraint_cache: (int * constraint_type * int, Formula.t) Hashtbl.t;
  invariants_cache: MPRF_Invariants.invariants_cache;
}

let new_cache max_depth =
  let new_template_table () = ref (List.init max_depth (fun i -> TemplateTable.create 10)) in
  {
    ranking_table = RankingTable.create 10;
    template_table = new_template_table ();
    template_table_real = new_template_table ();
    coeffs_table = CoeffsTable.create 10;
    coeffs_table_real = CoeffsTable.create 10;
    constraint_cache = Hashtbl.create 10;
    invariants_cache = MPRF_Invariants.new_cache ();
  }

(* Cache does not depend on measure since the cache is unique for each measure *)
let constraint_cache cache =
  Util.memoize_v2 cache.constraint_cache ~extractor:(fun (depth, _, constraint_type, t) -> depth, constraint_type, Transition.id t)

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

let as_parapoly_real label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> RealParameterPolynomial.of_var var
  | Some p -> RealParameterPolynomial.of_intpoly p

(** Given a list of variables an affine template-polynomial is generated*)
let ranking_template_real cache location (vars: VarSet.t): RealParameterPolynomial.t * Var.t list =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Real num_vars in
  let fresh_coeffs = List.map RealPolynomial.of_var fresh_vars in

  (* store fresh_vars *)
  let coeff_table = cache.coeffs_table_real in
  List.iter
    (fun (v,v') -> CoeffsTable.modify_def VarSet.empty (location,v) (VarSet.union (VarSet.singleton v')) coeff_table)
    (List.combine vars fresh_vars);

  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Real () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly),
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

let polyList_to_string ((rank: (Location.t -> 'a) list) , (l : Location.t)) =
  rank
  |> List.enum
  |> Util.enum_to_string (fun p -> Polynomial.to_string(p l) ^ " ")

let only_rank_to_string {rank; decreasing; non_increasing; depth} =
  let locations = non_increasing |> TransitionSet.enum |> Program.locations |> List.of_enum |> List.unique ~eq:Location.equal in
  rank_to_string locations polyList_to_string rank

let to_string {rank; decreasing; non_increasing; depth} =
  "{multirank:" ^ only_rank_to_string {rank; decreasing; non_increasing; depth} ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"

let fresh_coeffs: Var.t list ref = ref []

let compute_ranking_templates_ (depth: int) (vars: VarSet.t) (locations: Location.t Enum.t) ranking_template_ template_table_ to_string: unit =
  let execute (i:int) =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = ranking_template_ location vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.of_enum @@ Enum.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> TemplateTable.add (List.nth !template_table_ i) location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars) -> fresh_vars)
    |> List.flatten
    |> (fun fresh_vars -> fresh_coeffs := fresh_vars)
  in
  for i = 0 to depth - 1 do
    Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_mprf_templates_" ^ string_of_int i, [])
      ~result:(fun () ->
          (List.nth !template_table_ i)
          |> TemplateTable.enum
          |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ to_string polynomial)
        )
      (fun () -> execute i);
  done

(* We do not set fresh_coeffs as we do not minimize *)
(* TODO Merge with compute_ranking_tempaltes_ *)
let compute_ranking_templates_real (depth: int) (vars: VarSet.t) (locations: Location.t Enum.t) ranking_template_ template_table_ to_string: unit =
  let execute (i:int) =
    (* Each location needs its own ranking template with different fresh variables *)
    let ins_loc_prf loc = ranking_template_ loc vars in

    locations
    |> Enum.iter
        (fun loc -> let (polynomial,_) = ins_loc_prf loc in TemplateTable.add (List.nth !template_table_ i) loc polynomial);
  in
  for i = 0 to depth - 1 do
    Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_mprf_templates_" ^ string_of_int i, [])
      ~result:(fun () ->
          (List.nth !template_table_ i)
          |> TemplateTable.enum
          |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ to_string polynomial)
        )
      (fun () -> execute i);
  done

let compute_ranking_templates cache (depth: int) (vars: VarSet.t) (locations: Location.t Enum.t) : unit =
  compute_ranking_templates_ depth vars locations (ranking_template cache) (cache.template_table) ParameterPolynomial.to_string

let compute_ranking_templates_real cache (depth: int) (vars: VarSet.t) (locations: Location.t Enum.t) : unit =
  compute_ranking_templates_real depth vars locations (ranking_template_real cache) (cache.template_table_real) RealParameterPolynomial.to_string

(* Methods define properties of mprf *)

(* method for mprf and functions f_2 to f_d of depth i *)
let transition_constraint_i (template_table0, template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template0 = TemplateTable.find template_table0 in
  let template1 = TemplateTable.find template_table1 in
  let poly = ParameterPolynomial.add (template0 l) (template1 l) in
  let atom =
    match constraint_type with
    | `Non_Increasing -> ParameterAtom.Infix.(poly >= ParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
    | `Decreasing -> ParameterAtom.Infix.(poly >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

(* method for mprf and function f_1*)
let transition_constraint_1 (template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template1 = TemplateTable.find template_table1 in
  let atom =
    match constraint_type with
      | `Non_Increasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
      | `Decreasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

(* method for mprf and function f_d*)
let transition_constraint_d bound (template_table1, measure, constraint_type, (l,t,l')) : Formula.t =
  let template1 = TemplateTable.find template_table1 in
    match constraint_type with
    | `Non_Increasing -> Formula.mk_true
    | `Decreasing  -> (
      let atom = ParameterAtom.Infix.((template1 l)  >= bound) in
        ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
        |> Formula.mk)

(* use all three functions above combined*)
let transition_constraint_ cache (depth, measure, constraint_type, (l,t,l')): Formula.t =
  let res = ref Formula.mk_true in
    for i = 1 to (depth - 1) do
      res := ((List.nth !(cache.template_table) (i - 1)), (List.nth !(cache.template_table) i), measure, constraint_type, (l,t,l'))
            |> transition_constraint_i
            |> Formula.mk_and !res
    done;
    res := ((List.nth !(cache.template_table) 0), measure, constraint_type, (l,t,l'))
          |> transition_constraint_1
          |> Formula.mk_and !res;
    if depth > 1 then
      res := ((List.nth !(cache.template_table) (depth - 1)), measure, constraint_type, (l,t,l'))
          |> transition_constraint_d ParameterPolynomial.zero
          |> Formula.mk_and !res
    else
      res := ((List.nth !(cache.template_table) (depth - 1)), measure, constraint_type, (l,t,l'))
          |> transition_constraint_d ParameterPolynomial.one
          |> Formula.mk_and !res;

    !res

let transition_constraint cache = constraint_cache cache (transition_constraint_ cache)

let transitions_constraint cache depth measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint cache (depth, measure, constraint_type, t) )
  |> Formula.all

let non_increasing_constraint cache depth measure transition =
  transition_constraint cache (depth, measure, `Non_Increasing, transition)

let non_increasing_constraints cache depth measure transitions =
  transitions_constraint cache depth measure `Non_Increasing (TransitionSet.to_list transitions)

let decreasing_constraint cache depth measure transition =
  transition_constraint cache (depth, measure, `Decreasing, transition)

(** A valuation is a function which maps from a finite set of variables to values *)

let rank_from_valuation cache depth (i: int) valuation location =
  location
  |> TemplateTable.find (List.nth !(cache.template_table) i)
  |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)

let rank_from_valuation_real  template valuation location =
  location
  |> template
  |> RealParameterPolynomial.eval_coefficients (fun var -> MPRF_Invariants.Valuation.eval_opt var valuation |? OurFloat.zero)
  |> RealPolynomial.to_intpoly

let make cache depth decreasing_transition non_increasing_transitions valuation  =
{
  rank = List.init depth (fun i -> rank_from_valuation cache depth i valuation);
  decreasing = decreasing_transition;
  non_increasing = non_increasing_transitions;
  depth = depth;
}

let make_inv cache depth decreasing_transition non_increasing_transitions valuation =
  {
    rank = List.init depth (fun i -> rank_from_valuation_real (TemplateTable.find (List.nth !(cache.template_table_real) i)) valuation);
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
    depth = depth;
  }


(** We are searching for a real model, hence we need to cast reals to integers. *)
let change_valuation (values: RealPolynomial.valuation) =
  Valuation.from (List.map (fun x -> (x,  MPRF_Invariants.Valuation.eval x values |> OurFloat.upper_int)) (MPRF_Invariants.Valuation.vars values))

let try_decreasing cache ?(inv = false) (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t) (non_increasing: Transition.t Stack.t)
   (to_be_found: int ref) applied_cfr (entry_transitions: TransitionSet.t) problem =
  non_increasing
  |> Stack.enum
  |> TransitionSet.inter problem.make_decreasing % TransitionSet.of_enum
  |> TransitionSet.enum
  |> Enum.filter (fun t -> not (RankingTable.mem cache.ranking_table t))
  |> Enum.iter (fun decreasing ->
        let current_time = Unix.gettimeofday() in
        Logger.(log logger DEBUG (fun () -> "try_decreasing", ["measure", show_measure problem.measure;
                                                               "decreasing", Transition.to_id_string decreasing;
                                                               "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                                               "depth", string_of_int problem.find_depth]));
        SMTSolverInt.push solver_int;

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

        VarSet.iter (SMTSolverInt.add solver_int % Formula.mk_eq Polynomial.zero % Polynomial.of_var)
          (unbounded_vars_at_entry_locs cache.coeffs_table);
        if inv then (
          SMTSolver.push solver_real;
          let templates_real = List.init problem.find_depth (fun n -> TemplateTable.find (List.nth !(cache.template_table_real) n)) in
          SMTSolver.add_real solver_real
            (MPRF_Invariants.decreasing_constraint cache.invariants_cache problem.find_depth problem.measure decreasing templates_real);
          SMTSolver.add_real solver_real
            (MPRF_Invariants.consecution_constraint cache.invariants_cache problem.find_depth problem.measure decreasing templates_real);

          (* set coefficients of unbounded variables to 0*)
          VarSet.iter (SMTSolverInt.add solver_int % Formula.mk_eq Polynomial.zero % Polynomial.of_var)
            (unbounded_vars_at_entry_locs cache.coeffs_table_real);

          TransitionSet.remove decreasing (TransitionTable.find problem.map_trans_pre_trans decreasing)
          |> TransitionSet.iter
            (fun trans ->  SMTSolver.add_real solver_real
              (MPRF_Invariants.initiation_constraint cache.invariants_cache problem.find_depth problem.measure trans templates_real)));

        SMTSolverInt.add solver_int (decreasing_constraint cache problem.find_depth problem.measure decreasing);

        if SMTSolverInt.satisfiable solver_int then (
          (* SMTSolverInt.minimize_absolute solver_int !fresh_coeffs; *)
          SMTSolverInt.model solver_int
          |> Option.map (make cache problem.find_depth decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum))
          |> Option.may (fun ranking_function ->
              to_be_found := !to_be_found - 1;
              RankingTable.add cache.ranking_table decreasing ranking_function;
              Logger.(log logger INFO (fun () -> "add_mprf", [
                                            "measure", show_measure problem.measure;
                                            "decreasing", Transition.to_id_string decreasing;
                                            "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                            "rank", only_rank_to_string ranking_function])))
        )
        else if inv && SMTSolver.satisfiable solver_real then (
          SMTSolver.model_real solver_real
           |> Option.map (
              make_inv cache problem.find_depth decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum)
              % tap (fun x -> MPRF_Invariants.store_inv cache.invariants_cache x decreasing)
              % tap (fun x -> MPRF_Invariants.store_inv_set cache.invariants_cache x (non_increasing |> Stack.enum |> TransitionSet.of_enum)))
          |> Option.may (fun ranking_function ->
              to_be_found := !to_be_found - 1;
              RankingTable.add cache.ranking_table decreasing ranking_function;
              Logger.(log logger INFO (fun () -> "add_mprf_inv", [
                  "measure", show_measure problem.measure;
                  "decreasing", Transition.to_id_string decreasing;
                  "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                  "rank", only_rank_to_string ranking_function];))
            )
        );

        SMTSolverInt.pop solver_int;
        if inv then
          SMTSolver.pop solver_real;

        if applied_cfr then (
          CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.gettimeofday() -. current_time);
          CFR.poll_timeout ~applied_cfr:applied_cfr)
    );
  if !to_be_found <= 0 then
    raise Exit


let entry_transitions_from_non_increasing map_trans_pre_trans non_increasing =
  let all_possible_pre_trans =
    Stack.enum non_increasing
    |> Enum.fold (fun tset t -> TransitionSet.union tset @@ TransitionTable.find map_trans_pre_trans t) TransitionSet.empty
  in
  TransitionSet.diff all_possible_pre_trans (TransitionSet.of_enum @@ Stack.enum non_increasing)


let rec backtrack cache ?(inv = false) (steps_left: int) (index: int) (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t)
  (non_increasing: Transition.t Stack.t) (to_be_found: int ref) applied_cfr problem =
    let try_decreasing_if_entrytime_bounded non_increasing =
      let entry_trans = entry_transitions_from_non_increasing problem.map_trans_pre_trans non_increasing in
      if TransitionSet.for_all problem.is_time_bounded entry_trans then
        try_decreasing cache ~inv:inv solver_real solver_int non_increasing to_be_found applied_cfr entry_trans problem;
    in

    if SMTSolverInt.satisfiable solver_int || (inv && SMTSolver.satisfiable solver_real) then (
      if steps_left == 0 then (
        try_decreasing_if_entrytime_bounded non_increasing
      ) else (
        for i=index to Array.length problem.scc - 1 do
          let transition = Array.get problem.scc i in

          SMTSolverInt.push solver_int;
          if inv then (
            SMTSolver.push solver_real;
            let templates_real = List.init problem.find_depth (fun n -> TemplateTable.find (List.nth !(cache.template_table_real) n)) in
            SMTSolver.add_real solver_real
              (MPRF_Invariants.non_increasing_constraint cache.invariants_cache problem.find_depth problem.measure transition templates_real);
            SMTSolver.add_real solver_real
              (MPRF_Invariants.consecution_constraint cache.invariants_cache problem.find_depth problem.measure transition templates_real);
            TransitionSet.remove transition (TransitionTable.find problem.map_trans_pre_trans transition)
            |> TransitionSet.iter
              (fun trans ->  SMTSolver.add_real solver_real
                (MPRF_Invariants.initiation_constraint cache.invariants_cache problem.find_depth problem.measure trans templates_real)));

          SMTSolverInt.add solver_int (non_increasing_constraint cache problem.find_depth problem.measure transition);
          Stack.push transition non_increasing;
          backtrack cache ~inv:inv (steps_left - 1) (i + 1) solver_real solver_int non_increasing
            to_be_found applied_cfr problem;
          ignore (Stack.pop non_increasing);

          SMTSolverInt.pop solver_int;
          if inv then
            SMTSolver.pop solver_real;
        done;
        try_decreasing_if_entrytime_bounded non_increasing
      )
    )

let compute_scc ?(inv = false) cache program applied_cfr mprf_problem =
  let locations = LocationSet.enum @@ TransitionGraph.locations (Program.graph program) in
  let vars = Program.input_vars program in
  try
    compute_ranking_templates cache mprf_problem.find_depth vars locations;
    if inv then
      compute_ranking_templates_real cache mprf_problem.find_depth vars locations;
      backtrack cache
                ~inv:inv
                (Array.length mprf_problem.scc)
                0
                (SMTSolver.create ())
                (SMTSolverInt.create ())
                (Stack.create ())
                (ref (TransitionSet.cardinal mprf_problem.make_decreasing))
                applied_cfr
                mprf_problem;
    mprf_problem.scc
    |> Array.iter (fun t ->
          if not (RankingTable.mem cache.ranking_table t) then
            Logger.(log logger WARN (fun () -> "no_mprf", ["measure", show_measure mprf_problem.measure; "transition", Transition.to_id_string t]))
        )
  with Exit -> ()

let compute_ cache ?(inv = false) measure applied_cfr program is_time_bounded unbounded_vars depth =
  let map_trans_pre_trans scc =
    let t = TransitionTable.create 10 in
    TransitionSet.enum scc
    |> Enum.map (fun t -> t, TransitionSet.of_enum (Program.pre program t))
    |> Enum.iter (uncurry @@ TransitionTable.add t) (* somehow find behaves strangely in combination with of_enum *)
    |> const t
  in
  let problem scc =
    {  map_trans_pre_trans = map_trans_pre_trans scc; measure = measure; scc = Array.of_enum (TransitionSet.enum scc); make_decreasing = scc;
       unbounded_vars; is_time_bounded; find_depth = depth; }
  in
  Program.sccs program
  |> Enum.iter (fun scc -> compute_scc cache ~inv program applied_cfr (problem scc))

let logging measure methode_name =
  Logger.with_log logger Logger.DEBUG
                  (fun () -> methode_name, ["measure", show_measure measure])
                  ~result:(Util.enum_to_string to_string % Enum.clone)

let find ?(inv = false) measure applied_cfr program depth =
  let cache = new_cache 5 in
  let execute () =
    if inv then
        MPRF_Invariants.compute_invariant_templates cache.invariants_cache (Program.input_vars program)
          (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);

    compute_ cache ~inv:inv measure applied_cfr program (const true) (const @@ VarSet.empty) depth;
    RankingTable.values cache.ranking_table
  in
  logging measure "find_mprf" execute

let find_scc ?(inv = false) measure applied_cfr program map_trans_pre_trans is_time_bounded unbounded_vars make_decreasing scc depth =
  let cache = new_cache depth in
  let mprf_problem =
    { map_trans_pre_trans; measure; scc = TransitionSet.to_array scc; make_decreasing; unbounded_vars; find_depth = depth; is_time_bounded}
  in
  let execute () =
    if inv then
      MPRF_Invariants.compute_invariant_templates cache.invariants_cache (Program.input_vars program)
        (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);

    compute_scc cache ~inv:inv program applied_cfr mprf_problem;
    RankingTable.values cache.ranking_table
  in
  logging measure "find_scc_mprf" execute

 module Dijkstra =
  Graph.Path.Dijkstra
    (TransitionGraph)
    (struct
      type edge = Transition.t
      type t = int
      let weight = const 1
      let compare = compare
      let add = (+)
      let zero = 0
    end)

module TransitionMap = Map.Make(
  struct
    type t = Transition.t
    let compare = Transition.compare_same
  end)

(* Compute the shortest path from a node to itself while not considering self-loops *)
let shortest_self_path_no_loop graph l =
  TransitionGraph.locations graph
  |> LocationSet.enum
  |> Enum.filter (not % Location.equal l)
  |> Enum.map
      (* Intermediate node not equal to l *)
      (fun v ->
          let composed_path () =
            let (p1, l1) = Dijkstra.shortest_path graph l v in
            let (p2, l2) = Dijkstra.shortest_path graph v l in
            Some (p1@p2, l1+l2)
          in
          try
            composed_path ()
          with Not_found -> None
      )
  |> Util.cat_maybes_enum
  |> List.of_enum

(* *)
let get_dijkstra_shortest_paths graph l1 l2  =
  try
    let (path, length) = Dijkstra.shortest_path graph l1 l2 in
    let loc_path = List.map Transition.target path in
    (* this list contains lists of transitions that are parallel and connect the corresponding nodes of loc_path *)
    let all_parallel_trans =
      Tuple2.first @@ List.fold_left
        (fun (ps, curr_loc) next_loc ->
          let par_trans = TransitionGraph.find_all_edges graph curr_loc next_loc in
          (List.append ps (List.singleton par_trans), next_loc)
        )
        ([], l1)
        loc_path
    in
    (* all possible parallel paths along loc_path *)
    let all_par_paths =
      List.fold_left (fun ps next_ts -> ListMonad.(next_ts >>= fun t -> List.map (fun p -> p@[t]) ps))
        [[]]
        all_parallel_trans
    in
    all_par_paths
    |> List.enum
    |> Enum.map (fun path -> path, length)
  with Not_found -> Enum.empty ()

let next_candidates not_added_graph graph =
  let already_connected_locations = TransitionGraph.locations graph |> LocationSet.enum in
  (*
    We want to close circles as fast as possible. Hence for all pairs of already added locations
    l1 and l2, check how many transitions of the non added transitions must be added to obtain
    a path l1 -> l2.
  *)
  Enum.cartesian_product (Enum.clone already_connected_locations) (Enum.clone already_connected_locations)
  |> Enum.map
      (fun (l1,l2) ->
(*         Dijkstra does not deal with self-loops *)
        if not (Location.equal l1 l2) then
          get_dijkstra_shortest_paths not_added_graph l1 l2
        else
          TransitionGraph.find_all_edges not_added_graph l1 l2
          |> List.enum
          |> Enum.map (fun t -> [t], 1)
          |> Enum.append (List.enum @@ shortest_self_path_no_loop not_added_graph l1)
      )
  |> Enum.flatten
  (* Which transition now is the begin of a path l1->l2? How long is a shortest such path? How many exist there?*)
  |> Enum.fold
      (fun tmap (path,length) ->
        let first_trans = List.hd path in
        TransitionMap.modify_def
          ([], Int.max_num) first_trans
          (fun (paths,length_shortest) -> path::paths, Int.min length length_shortest) tmap
      )
      TransitionMap.empty
  |> TransitionMap.enum
  |> List.of_enum
  |> List.sort
      (fun (gt1,(paths1,min_length1)) (gt,(paths2,min_length2))->
        compare (min_length1, List.length paths1) (min_length2, List.length paths2))
  |> List.map (fst % snd)
  |> List.flatten
  |> tap (fun l ->
        Logger.log logger Logger.DEBUG
        (fun () -> "next_candidates",
          ["candidates", Util.enum_to_string (Util.enum_to_string Transition.to_id_string) % List.enum @@ List.map List.enum l]
        )
      )

(* We first try this without inv enabled and if we arent successful we retry with inv on. *)
let close_next_cycle cache depth ?(inv = false) program measure (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t) not_added_graph graph: TransitionSet.t =
  let all_paths = next_candidates not_added_graph graph in
  let rec close_circle path: bool * TransitionSet.t = match path with
    | []    -> true, TransitionSet.empty
    | e::es ->
        if inv then
          SMTSolver.push solver_real
        else
          SMTSolverInt.push solver_int;

        if inv then
          let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !(cache.template_table_real) n)) in
          SMTSolver.add_real solver_real
            (MPRF_Invariants.non_increasing_constraint cache.invariants_cache depth measure e templates_real);
          SMTSolver.add_real solver_real
            (MPRF_Invariants.consecution_constraint cache.invariants_cache depth measure e templates_real);
          Program.entry_transitions logger program [e]
          |> List.iter (fun trans ->  SMTSolver.add_real solver_real
                (MPRF_Invariants.initiation_constraint cache.invariants_cache depth measure trans templates_real))
        else
          SMTSolverInt.add solver_int (non_increasing_constraint cache depth measure e);

        if (not inv && SMTSolverInt.satisfiable solver_int) || (inv && SMTSolver.satisfiable solver_real) then
          let (closed, non_inc_set) = close_circle es in
          if inv then
            SMTSolver.pop solver_real
          else
            SMTSolverInt.pop solver_int;
          closed, TransitionSet.add e non_inc_set
        else (
          if inv then
            SMTSolver.pop solver_real
          else
            SMTSolverInt.pop solver_int;
          false, TransitionSet.empty)
  in
  let rec choose_circle paths tset = match paths with
    | []    -> tset
    | p::ps ->
        let (closed_curr, tset_curr) = close_circle p in
        if closed_curr then
          tset_curr
        else
          let best_tset =
            if TransitionSet.cardinal tset >= TransitionSet.cardinal tset_curr then
              tset
            else
              tset_curr
          in
          choose_circle ps best_tset
  in
  choose_circle all_paths TransitionSet.empty
  |> tap (fun tset -> Logger.log logger Logger.DEBUG (fun () -> "close_next_cycle", ["add to non_inc_tset", TransitionSet.to_string tset]))
  (* Add Constraints to the SMTSolver *)
  |> tap (
      TransitionSet.iter (fun transition ->
      if inv then
        let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !(cache.template_table_real) n)) in
          SMTSolver.add_real solver_real
            (MPRF_Invariants.non_increasing_constraint cache.invariants_cache depth measure transition templates_real);
          SMTSolver.add_real solver_real
            (MPRF_Invariants.consecution_constraint cache.invariants_cache depth measure transition templates_real);
          Program.entry_transitions logger program [transition]
          |> List.iter (fun trans ->  SMTSolver.add_real solver_real
                (MPRF_Invariants.initiation_constraint cache.invariants_cache depth measure trans templates_real))
      else
        SMTSolverInt.add solver_int (non_increasing_constraint cache depth measure transition);)
      )


let find_non_inc_set cache depth ?(inv = false) program measure (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t) decreasing try_non_inc_set =
  let decr_graph =
    TransitionGraph.empty
    |> flip TransitionGraph.add_edge_e decreasing
  in
  let not_added_graph =
    TransitionGraph.empty
    |> TransitionSet.fold (flip TransitionGraph.add_edge_e) try_non_inc_set
  in
  let rec try_close_cycles n_a_g g =
    let next_non_inc_set = close_next_cycle ~inv:inv cache depth program measure solver_real solver_int n_a_g g in
    if TransitionSet.is_empty next_non_inc_set then
      TransitionGraph.transitions g
    else
      let n_a_g' = TransitionSet.fold (flip TransitionGraph.remove_edge_e) next_non_inc_set n_a_g in
      let g' = TransitionSet.fold (flip TransitionGraph.add_edge_e) next_non_inc_set g in
      try_close_cycles n_a_g' g'
  in
  try_close_cycles not_added_graph decr_graph

let compute_and_add_ranking_function cache ?(inv = false) applied_cfr program measure all_trans depth decreasing : unit =
  let current_time = Unix.gettimeofday () in
  try
    compute_ranking_templates cache depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.enum);
    if inv then
      compute_ranking_templates_real cache depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.enum);
    let try_non_inc_set = TransitionSet.remove decreasing all_trans in
    let solver_int = SMTSolverInt.create () in
    let solver_real = SMTSolver.create () in
    if inv then (
      let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !(cache.template_table_real) n)) in
      SMTSolver.add_real solver_real
        (MPRF_Invariants.decreasing_constraint cache.invariants_cache depth measure decreasing templates_real);
      SMTSolver.add_real solver_real
        (MPRF_Invariants.consecution_constraint cache.invariants_cache depth measure decreasing templates_real);
      Program.entry_transitions logger program [decreasing]
      |> List.iter (fun trans ->  SMTSolver.add_real solver_real
            (MPRF_Invariants.initiation_constraint cache.invariants_cache depth measure trans templates_real)));

    SMTSolverInt.add solver_int (decreasing_constraint cache depth measure decreasing);
    if SMTSolverInt.satisfiable solver_int then (
      (* First without invariants *)
      let non_inc = find_non_inc_set ~inv:false cache depth program measure solver_real solver_int decreasing try_non_inc_set in
      (* SMTSolverInt.minimize_absolute solver_int !fresh_coeffs; *)
      SMTSolverInt.model solver_int
      |> Option.map (make cache depth decreasing non_inc)
      |> Option.may (fun rank_func ->
        RankingTable.add cache.ranking_table decreasing rank_func;
        Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                      "measure", show_measure measure;
                                      "decreasing", Transition.to_id_string decreasing;
                                      "non_increasing", TransitionSet.to_string non_inc;
                                      "rank", only_rank_to_string rank_func])));
      raise Exit
    );
    if inv && SMTSolver.satisfiable solver_real then (
      let non_inc = find_non_inc_set ~inv:inv cache depth program measure solver_real solver_int decreasing try_non_inc_set in
      if inv then
      SMTSolver.model_real solver_real
        |> Option.map (
          make_inv cache depth decreasing non_inc
          % tap (fun x -> MPRF_Invariants.store_inv cache.invariants_cache x decreasing)
          % tap (fun x -> MPRF_Invariants.store_inv_set cache.invariants_cache x non_inc))
      |> Option.may (fun rank_func ->
        RankingTable.add cache.ranking_table decreasing rank_func;
        Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                      "measure", show_measure measure;
                                      "decreasing", Transition.to_id_string decreasing;
                                      "non_increasing", TransitionSet.to_string non_inc;
                                      "rank", only_rank_to_string rank_func])));
      raise Exit
    )
    with Exit -> (
      if applied_cfr then (
        CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.gettimeofday() -. current_time);
        CFR.poll_timeout ~applied_cfr:applied_cfr))

let compute_fast cache ?(inv = false) measure applied_cfr program max_depth =
  program
  |> Program.sccs
  |> Enum.iter (fun scc -> TransitionSet.iter (compute_and_add_ranking_function ~inv:inv cache applied_cfr program measure scc max_depth) scc)

let compute_scc_fast cache ?(inv = false) measure applied_cfr program max_depth scc =
  scc
  |> TransitionSet.iter (compute_and_add_ranking_function ~inv:inv cache applied_cfr program measure scc max_depth)

let find_fast ?(inv = false) measure applied_cfr program max_depth =
  let execute () =
    let cache = new_cache max_depth in
    if inv then
      MPRF_Invariants.compute_invariant_templates cache.invariants_cache (Program.input_vars program)
        (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);

    compute_fast cache ~inv:inv measure applied_cfr program max_depth;
    RankingTable.values cache.ranking_table
  in
  logging measure "find_ranking_functions_fast" execute

let find_scc_fast ?(inv = false) measure applied_cfr program scc max_depth =
  let execute () =
    let cache = new_cache max_depth in
    if inv then
      MPRF_Invariants.compute_invariant_templates cache.invariants_cache (Program.input_vars program)
        (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);


    compute_scc_fast cache ~inv:inv measure applied_cfr program max_depth scc;
    RankingTable.values cache.ranking_table
  in
  logging measure "find_ranking_functions_scc_fast" execute
