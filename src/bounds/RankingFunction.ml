open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open CFR

module SMTSolver = SMT.IncrementalZ3Solver
module SMTSolverInt = SMT.SolverFast
module Valuation = Valuation.Make(OurInt)

type t = {
    rank : Location.t -> Polynomial.t;
    decreasing : Transition.t;
    non_increasing : TransitionSet.t;
  }

type measure = [ `Cost | `Time ] [@@deriving show, eq]

type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded ] [@@deriving show, eq]

module TemplateTable = Hashtbl.Make(Location)
module RankingTable = Hashtbl.Make(struct include Transition let equal = Transition.same end)
module CoeffTable = Hashtbl.Make(struct
                                   type t = Location.t * Var.t
                                   let equal (l1,v1) (l2,v2) = Location.equal l1 l2 && Var.equal v1 v2
                                   let hash = Hashtbl.hash
                                 end)

type ranking_cache = (t RankingTable.t * t RankingTable.t * Polynomials.ParameterPolynomial.t TemplateTable.t *
  RealParameterPolynomial.t TemplateTable.t * (Var.t CoeffTable.t * Var.t CoeffTable.t))
let new_cache: unit -> ranking_cache =
  fun () -> (RankingTable.create 10, RankingTable.create 10, TemplateTable.create 10, TemplateTable.create 10, (CoeffTable.create 10, CoeffTable.create 10))

let get_time_ranking_table     (cache: ranking_cache) = Tuple5.first cache
let get_cost_ranking_table     (cache: ranking_cache) = Tuple5.second cache
let get_template_table         (cache: ranking_cache) = Tuple5.third cache
let get_template_table_real    (cache: ranking_cache) = Tuple5.fourth cache
let get_coeff_table            (cache: ranking_cache) = Tuple2.first @@ Tuple5.fifth cache
let get_coeff_table_real       (cache: ranking_cache) = Tuple2.second @@ Tuple5.fifth cache

let constraint_cache = Util.cache ~extractor:(fun (_, measure, constraint_type, t) -> (measure, constraint_type, Transition.id t))

let get_ranking_table measure =
  match measure with
  | `Time -> get_time_ranking_table
  | `Cost -> get_cost_ranking_table

let one = ParameterPolynomial.one

let logger = Logging.(get PRF)

let rank f = f.rank

let decreasing f = f.decreasing

let non_increasing f = f.non_increasing

let rank_to_string (locations: Location.t list) (content_to_string: 'a -> string) (pol: Location.t -> 'a) =
  locations
  |> List.enum
  |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (pol l))

let only_rank_to_string {rank; decreasing; non_increasing} =
  let locations = non_increasing |> TransitionSet.enum |> Program.locations |> List.of_enum |> List.unique ~eq:Location.equal in
  rank_to_string locations Polynomial.to_string rank

let to_string {rank; decreasing; non_increasing} =
  "{rank:" ^ only_rank_to_string {rank; decreasing; non_increasing} ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"

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
  let coeff_table = get_coeff_table cache in
  List.iter
    (fun (v,v') -> CoeffTable.add coeff_table (location,v) v')
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
  let coeff_table = get_coeff_table cache in
  List.iter
    (fun (v,v') -> CoeffTable.add coeff_table (location,v) v')
    (List.combine vars fresh_vars);

  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Real () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var]

let fresh_coeffs: Var.t list ref = ref []

let compute_ranking_templates_ (vars: VarSet.t) (locations: Location.t list) ranking_template_ template_table_ to_string: unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = ranking_template_ location vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> TemplateTable.add template_table_ location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars)-> fresh_vars)
    |> List.flatten
    |> (fun fresh_vars -> fresh_coeffs := fresh_vars)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_ranking_templates", [])
                  ~result:(fun () ->
                    template_table_
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ to_string polynomial)
                  )
                  execute

(* We dont set fresh_coeffs as we do not minimize *)
let compute_ranking_templates_real (vars: VarSet.t) (locations: Location.t list) ranking_template_ template_table_ to_string: unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = ranking_template_ location vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> TemplateTable.add template_table_ location polynomial);
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_ranking_templates", [])
                  ~result:(fun () ->
                    template_table_
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ to_string polynomial)
                  )
                  execute

let compute_ranking_templates cache (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_ vars locations (ranking_template cache) (get_template_table cache) ParameterPolynomial.to_string

let compute_ranking_templates_real cache (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_real vars locations (ranking_template_real cache) (get_template_table_real cache) RealParameterPolynomial.to_string

let decreaser measure t =
  match measure with
  | `Cost -> TransitionLabel.cost t
  | `Time -> Polynomial.one

let transition_constraint_ (cache, measure, constraint_type, (l,t,l')): Formula.t =
  let template = TemplateTable.find (get_template_table cache) in
  let atom =
    match constraint_type with
    | `Non_Increasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.substitute_f (as_parapoly t) (template l'))
    | `Decreasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.(of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template l')))
    | `Bounded -> ParameterAtom.Infix.(template l >= ParameterPolynomial.of_polynomial (decreaser measure t))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

let transition_constraint = constraint_cache#add transition_constraint_

let transitions_constraint cache measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint (cache, measure, constraint_type, t))
  |> Formula.all

let non_increasing_constraint cache measure transition =
  transition_constraint (cache, measure, `Non_Increasing, transition)

let non_increasing_constraints cache measure transitions =
  transitions_constraint cache measure `Non_Increasing (TransitionSet.to_list transitions)

let bounded_constraint cache measure transition =
  transition_constraint (cache, measure, `Bounded, transition)

let decreasing_constraint cache measure transition =
  transition_constraint (cache, measure, `Decreasing, transition)

let rank_from_valuation cache valuation location =
  location
  |> TemplateTable.find (get_template_table cache)
  |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)

let rank_from_valuation_real cache valuation location =
  location
  |> TemplateTable.find (get_template_table_real cache)
  |> RealParameterPolynomial.to_int_parapoly
  |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)

let make cache decreasing_transition non_increasing_transitions valuation =
  {
    rank = rank_from_valuation cache valuation;
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
  }

let make_inv cache decreasing_transition non_increasing_transitions valuation =
  {
    rank = rank_from_valuation_real cache valuation;
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
  }

(** We are searching for a real model, hence we need to cast reals to integers. *)
let change_valuation (values: RealPolynomial.valuation) =
  Valuation.from (List.map (fun x -> (x,  Invariants.Valuation.eval x values |> OurFloat.upper_int)) (Invariants.Valuation.vars values))

let try_decreasing cache ?(inv = false) (solver_real: SMTSolver.t)  (solver_int: SMTSolverInt.t) (non_increasing: Transition.t Stack.t)
  (possible_decreasing: TransitionSet.t) (to_be_found: int ref) (measure: measure) applied_cfr (unbounded_vars: Transition.t ->
    VarSet.t) entry_transitions entry_trans_map =
  non_increasing
  |> Stack.enum
  |> TransitionSet.inter possible_decreasing % TransitionSet.of_enum
  |> TransitionSet.enum
  |> Enum.filter (fun t -> not (RankingTable.mem (get_ranking_table measure cache) t))
  |> Enum.iter (fun decreasing ->
        let current_time = Unix.gettimeofday() in
        Logger.(log logger DEBUG (fun () -> "try_decreasing", ["measure", show_measure measure;
                                                              "decreasing", Transition.to_id_string decreasing;
                                                              "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing)]));
        SMTSolverInt.push solver_int;

        (* Set the coefficients for all variables for which a corresponding size bound does not exist for the entry transitions to
         * 0. *)
        let entry_trans_grouped_by_loc =
          List.sort (fun (_,_,l'1) (_,_,l'2) -> Location.compare l'1 l'2)  (TransitionSet.to_list entry_transitions)
          |> List.group_consecutive (fun (_,_,l'1) (_,_,l'2) -> Location.equal l'1 l'2)
        in
        let unbounded_vars_at_entry_locs =
          List.map
            (fun ts ->
              let entryloc = Transition.target (List.hd ts) in
              let coeff_table = get_coeff_table cache in
              List.enum ts
              |> Enum.map unbounded_vars
              |> Enum.fold VarSet.union VarSet.empty
              |> VarSet.map (fun v -> CoeffTable.find coeff_table (entryloc,v)))
            entry_trans_grouped_by_loc
          |> List.fold_left VarSet.union VarSet.empty
        in
        VarSet.iter (SMTSolverInt.add solver_int % Formula.mk_eq Polynomial.zero % Polynomial.of_var) unbounded_vars_at_entry_locs;


        if inv then (
          SMTSolver.push solver_real;
          let template = TemplateTable.find (get_template_table_real cache) in
          SMTSolver.add_real solver_real (Invariants.bounded_constraint measure decreasing template);
          SMTSolver.add_real solver_real (Invariants.decreasing_constraint measure decreasing template);
          SMTSolver.add_real solver_real (Invariants.consecution_constraint measure decreasing template);

          (* set coefficients of unbounded variables to 0*)
          VarSet.iter (SMTSolverInt.add solver_int % Formula.mk_eq Polynomial.zero % Polynomial.of_var) unbounded_vars_at_entry_locs;

          TransitionTable.find entry_trans_map decreasing
          |> TransitionSet.iter (fun trans ->  SMTSolver.add_real solver_real (Invariants.initiation_constraint measure trans template)));

        SMTSolverInt.add solver_int (bounded_constraint cache measure decreasing);
        SMTSolverInt.add solver_int (decreasing_constraint cache measure decreasing);

        if SMTSolverInt.satisfiable solver_int then (
          (* SMTSolverInt.minimize_absolute solver_int !fresh_coeffs; *)
          SMTSolverInt.model solver_int
          |> Option.map (make cache decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum))
          |> Option.may (fun ranking_function ->
              to_be_found := !to_be_found - 1;
              RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
              Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                            "measure", show_measure measure;
                                            "decreasing", Transition.to_id_string decreasing;
                                            "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                            "rank", only_rank_to_string ranking_function])))
        )
        else if inv && SMTSolver.satisfiable solver_real then (
          SMTSolver.model_real solver_real
          |> Option.map (
              make_inv cache decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum) % change_valuation
              % tap (fun x -> Invariants.store_inv x decreasing)
              % tap (fun x -> Invariants.store_inv_set x (non_increasing |> Stack.enum |> TransitionSet.of_enum)))
          |> Option.may (fun ranking_function ->
                to_be_found := !to_be_found - 1;
                RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
                Logger.(log logger INFO (fun () -> "add_ranking_function_inv", [
                                              "measure", show_measure measure;
                                              "decreasing", Transition.to_id_string decreasing;
                                              "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                              "rank", only_rank_to_string ranking_function]))
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

let entry_transitions_from_non_increasing map_trans_entry_trans non_increasing =
  let all_possible_entry_trans =
    Stack.enum non_increasing
    |> Enum.fold (fun tset t -> TransitionSet.union tset @@ TransitionTable.find map_trans_entry_trans t) TransitionSet.empty
  in
  TransitionSet.diff all_possible_entry_trans (TransitionSet.of_enum @@ Stack.enum non_increasing)

let rec backtrack cache ?(inv = false) (steps_left: int) (index: int) (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t)
                  (scc: Transition.t array) (non_increasing: Transition.t Stack.t) (possible_decreasing: TransitionSet.t)
                  (to_be_found: int ref) (measure: measure) applied_cfr is_time_bounded unbounded_vars
                  (map_trans_entry_trans: TransitionSet.t TransitionTable.t) =
  if SMTSolverInt.satisfiable solver_int || (inv && SMTSolver.satisfiable solver_real) then (
    if steps_left == 0 then (
      let entry_trans = entry_transitions_from_non_increasing map_trans_entry_trans non_increasing in
      try_decreasing cache ~inv:inv solver_real solver_int non_increasing possible_decreasing to_be_found measure
        applied_cfr unbounded_vars entry_trans map_trans_entry_trans
    ) else (
      for i=index to Array.length scc - 1 do
        let transition = Array.get scc i in

        SMTSolverInt.push solver_int;
        if inv then (
          SMTSolver.push solver_real;
          let template_real = TemplateTable.find (get_template_table_real cache) in
          SMTSolver.add_real solver_real (Invariants.non_increasing_constraint measure transition template_real);
          SMTSolver.add_real solver_real (Invariants.consecution_constraint measure transition template_real);
          TransitionTable.find map_trans_entry_trans transition
          |> TransitionSet.iter (fun trans ->  SMTSolver.add_real solver_real (Invariants.initiation_constraint measure trans template_real)));

        SMTSolverInt.add solver_int (non_increasing_constraint cache measure transition);
        Stack.push transition non_increasing;
        backtrack cache ~inv:inv (steps_left - 1) (i + 1) solver_real solver_int scc non_increasing possible_decreasing
          to_be_found measure applied_cfr is_time_bounded unbounded_vars map_trans_entry_trans;
        ignore (Stack.pop non_increasing);

        SMTSolverInt.pop solver_int;
        if inv then
          SMTSolver.pop solver_real;
      done;

      let entry_trans = entry_transitions_from_non_increasing map_trans_entry_trans non_increasing in
      (* Only try_decreasing if the non_increasing set looks good, i.e. all incoming timebounds are finite*)
      if TransitionSet.for_all is_time_bounded entry_trans then
        try_decreasing cache ~inv:inv solver_real solver_int non_increasing possible_decreasing to_be_found
          measure applied_cfr unbounded_vars entry_trans map_trans_entry_trans
    )
  )

let compute_scc ?(inv = false) cache measure applied_cfr map_trans_entry_trans is_time_bounded unbounded_vars decreasing_transitions scc =
  try
    backtrack cache
              (TransitionSet.cardinal scc)
              0
              (SMTSolver.create ())
              (SMTSolverInt.create ())
              (Array.of_enum (TransitionSet.enum scc))
              (Stack.create ())
              decreasing_transitions
              (ref (TransitionSet.cardinal decreasing_transitions))
              measure
              applied_cfr
              ~inv:inv
              is_time_bounded
              unbounded_vars
              map_trans_entry_trans;
    scc
    |> TransitionSet.iter (fun t ->
          if not (RankingTable.mem (get_ranking_table measure cache) t) then
            Logger.(log logger WARN (fun () -> "no_ranking_function", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
        )
  with Exit -> ()

let compute_ ?(inv = false) cache measure applied_cfr program is_time_bounded unbounded_vars =
  let map_trans_entry_trans scc =
    let t = TransitionTable.create 10 in
    TransitionSet.enum scc
    |> Enum.map (fun t -> t, TransitionSet.of_list (Program.entry_transitions logger program [t]))
    |> Enum.iter (uncurry @@ TransitionTable.add t) (* somehow find behaves strangely in combination with of_enum *)
    |> const t
  in
  program
  |> Program.sccs
  |> Enum.iter (fun scc -> compute_scc ~inv cache measure applied_cfr (map_trans_entry_trans scc) is_time_bounded unbounded_vars scc scc)


let fill_template_cache cache ?(inv = false) measure program applied_cfr =
  if inv then (
    if Invariants.template_table_is_empty then
      Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list););
    if TemplateTable.is_empty (get_template_table cache) then
      compute_ranking_templates cache (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if inv && TemplateTable.is_empty (get_template_table_real cache) then
      compute_ranking_templates_real cache (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list)

let logging measure transition methode_name =
  Logger.with_log logger Logger.DEBUG
                  (fun () -> methode_name, ["measure", show_measure measure;
                                                        "transition", Transition.to_id_string transition])
                  ~result:(Util.enum_to_string to_string % List.enum)

let find cache ?(inv = false) measure applied_cfr program transition =
  let execute () =
    fill_template_cache cache ~inv:inv measure program applied_cfr;
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_ cache measure applied_cfr program ~inv:inv (const true) (const @@ VarSet.empty);
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_ranking_functions" execute

let find_scc cache ?(inv = false) measure applied_cfr program map_trans_entry_trans transition is_time_bounded unbounded_vars decreasing_transitions scc =
  let execute () =
      fill_template_cache cache ~inv:inv measure program applied_cfr;
      if RankingTable.is_empty (get_ranking_table measure cache) then
        compute_scc cache measure applied_cfr map_trans_entry_trans ~inv:inv is_time_bounded unbounded_vars decreasing_transitions scc;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_ranking_functions_scc" execute


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
let close_next_cycle cache ?(inv = false) program measure (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t) not_added_graph graph: TransitionSet.t =
  let all_paths = next_candidates not_added_graph graph in
  let rec close_circle path: bool * TransitionSet.t = match path with
    | []    -> true, TransitionSet.empty
    | e::es ->
        if inv then
          SMTSolver.push solver_real
        else
          SMTSolverInt.push solver_int;

        if inv then
          let template_real = TemplateTable.find (get_template_table_real cache) in
          SMTSolver.add_real solver_real (Invariants.non_increasing_constraint measure e template_real);
          SMTSolver.add_real solver_real (Invariants.consecution_constraint measure e template_real);
          Program.entry_transitions logger program [e]
          |> List.iter (fun trans ->  SMTSolver.add_real solver_real (Invariants.initiation_constraint measure trans template_real))
        else
          SMTSolverInt.add solver_int (non_increasing_constraint cache measure e);
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
        let template_real = TemplateTable.find (get_template_table_real cache) in
        SMTSolver.add_real solver_real (Invariants.non_increasing_constraint measure transition template_real);
        SMTSolver.add_real solver_real (Invariants.consecution_constraint measure transition template_real);
        Program.entry_transitions logger program [transition]
        |> List.iter (fun trans ->  SMTSolver.add_real solver_real (Invariants.initiation_constraint measure trans template_real))
      else
        SMTSolverInt.add solver_int (non_increasing_constraint cache measure transition);)
      )


let find_non_inc_set cache ?(inv = false) program measure (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t) decreasing try_non_inc_set =
  let decr_graph =
    TransitionGraph.empty
    |> flip TransitionGraph.add_edge_e decreasing
  in
  let not_added_graph =
    TransitionGraph.empty
    |> TransitionSet.fold (flip TransitionGraph.add_edge_e) try_non_inc_set
  in
  let rec try_close_cycles n_a_g g =
    let next_non_inc_set = close_next_cycle ~inv:inv cache program measure solver_real solver_int n_a_g g in
    if TransitionSet.is_empty next_non_inc_set then
      TransitionGraph.transitions g
    else
      let n_a_g' = TransitionSet.fold (flip TransitionGraph.remove_edge_e) next_non_inc_set n_a_g in
      let g' = TransitionSet.fold (flip TransitionGraph.add_edge_e) next_non_inc_set g in
      try_close_cycles n_a_g' g'
  in
  try_close_cycles not_added_graph decr_graph


let compute_and_add_ranking_function cache ?(inv = false) applied_cfr program measure all_trans decreasing : unit =
  let current_time = Unix.gettimeofday () in
  try
    let try_non_inc_set = TransitionSet.remove decreasing all_trans in
    let solver_real = SMTSolver.create () in
    let solver_int = SMTSolverInt.create () in
    if inv then (
      let template = TemplateTable.find (get_template_table_real cache) in
      SMTSolver.add_real solver_real (Invariants.bounded_constraint measure decreasing template);
      SMTSolver.add_real solver_real (Invariants.decreasing_constraint measure decreasing template);
      SMTSolver.add_real solver_real (Invariants.consecution_constraint measure decreasing template);
      Program.entry_transitions logger program [decreasing]
      |> List.iter (fun trans ->  SMTSolver.add_real solver_real (Invariants.initiation_constraint measure trans template)));

    SMTSolverInt.add solver_int (bounded_constraint cache measure decreasing);
    SMTSolverInt.add solver_int (decreasing_constraint cache measure decreasing);
      if SMTSolverInt.satisfiable solver_int then (
        (* First without invariants *)
        let non_inc = find_non_inc_set ~inv:false cache program measure solver_real solver_int decreasing try_non_inc_set in
        (* SMTSolverInt.minimize_absolute solver_int !fresh_coeffs; *)
        SMTSolverInt.model solver_int
        |> Option.map (make cache decreasing non_inc)
        |> Option.may (fun ranking_function ->
            RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
            Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                          "measure", show_measure measure;
                                          "decreasing", Transition.to_id_string decreasing;
                                          "non_increasing", TransitionSet.to_string non_inc;
                                          "rank", only_rank_to_string ranking_function])));
        raise Exit
      );
      if inv && SMTSolver.satisfiable solver_real then (
        let non_inc = find_non_inc_set ~inv:inv cache program measure solver_real solver_int decreasing try_non_inc_set in
        SMTSolver.model_real solver_real
        |> Option.map (
            make_inv cache decreasing non_inc % change_valuation
            % tap (fun x -> Invariants.store_inv x decreasing)
            % tap (fun x -> Invariants.store_inv_set x non_inc))
        |> Option.may (fun ranking_function ->
              RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
              Logger.(log logger INFO (fun () -> "add_ranking_function_inv", [
                                            "measure", show_measure measure;
                                            "decreasing", Transition.to_id_string decreasing;
                                            "non_increasing", TransitionSet.to_string non_inc;
                                            "rank", only_rank_to_string ranking_function]))
            )
      );
    with Exit -> (
        if applied_cfr then (
          CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.gettimeofday() -. current_time);
          CFR.poll_timeout ~applied_cfr:applied_cfr))

let compute_fast cache ?(inv = false) measure applied_cfr program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc -> TransitionSet.iter (compute_and_add_ranking_function ~inv:inv cache applied_cfr program measure scc) scc)

let compute_scc_fast cache ?(inv = false) measure applied_cfr program scc =
  scc
  |> TransitionSet.iter (compute_and_add_ranking_function ~inv:inv cache applied_cfr program measure scc)

let find_fast cache ?(inv = false) measure applied_cfr program transition =
  let execute () =
    fill_template_cache cache ~inv:inv measure program applied_cfr;
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_fast cache ~inv:inv measure applied_cfr program;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_ranking_functions_fast" execute

let find_scc_fast cache ?(inv = false) measure applied_cfr program transition scc =
  let execute () =
    fill_template_cache cache ~inv:inv measure program applied_cfr;
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_scc_fast cache ~inv:inv measure applied_cfr program scc;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_ranking_functions_scc_fast" execute

let reset cache =
  constraint_cache#clear;
  Invariants.clear_cache;
  RankingTable.clear (get_time_ranking_table cache);
  RankingTable.clear (get_cost_ranking_table cache);
  Invariants.template_table_clear;
  TemplateTable.clear (get_template_table cache);
  TemplateTable.clear (get_template_table_real cache)
