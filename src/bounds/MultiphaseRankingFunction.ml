open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open CFR

(** Class is derived from RankingFunction.ml*)

module SMTSolver = SMT.IncrementalZ3Solver
module SMTSolverInt = SMT.IncrementalZ3SolverInt
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

let maxDepth = ref 5

type template_tables = ParameterPolynomial.t TemplateTable.t list Batteries.ref
type template_tables_real = RealParameterPolynomial.t TemplateTable.t list Batteries.ref

type ranking_cache = (t RankingTable.t * t RankingTable.t * template_tables * template_tables_real)
let new_cache: unit -> ranking_cache =
  fun () -> (RankingTable.create 10, RankingTable.create 10, ref (List.init !maxDepth (fun i -> TemplateTable.create 10)) , ref  (List.init !maxDepth (fun i -> TemplateTable.create 10)))

let constraint_cache = Util.cache ~extractor:(fun (_, depth, measure, constraint_type, t) -> (depth, measure, constraint_type, Transition.id t))

let get_time_ranking_table     (cache: ranking_cache) = Tuple4.first cache
let get_cost_ranking_table     (cache: ranking_cache) = Tuple4.second cache
let get_template_table         (cache: ranking_cache) = Tuple4.third cache
let get_template_table_real    (cache: ranking_cache) = Tuple4.fourth cache

let get_ranking_table measure = 
  match measure with
  | `Time -> get_time_ranking_table 
  | `Cost -> get_cost_ranking_table

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
let ranking_template (vars: VarSet.t): ParameterPolynomial.t * Var.t list =
    let vars = VarSet.elements vars in
    let num_vars = List.length vars in
    let fresh_vars = Var.fresh_id_list Var.Int num_vars in
    let fresh_coeffs = List.map Polynomial.of_var fresh_vars in
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
let ranking_template_real (vars: VarSet.t): RealParameterPolynomial.t * Var.t list =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Real num_vars in
  let fresh_coeffs = List.map RealPolynomial.of_var fresh_vars in
  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Real () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var]

let rank f = f.rank

let decreasing f = f.decreasing

let non_increasing f = TransitionSet.to_list f.non_increasing

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

let numberOfGeneratedTemplates = ref 0

let compute_ranking_templates_ (depth: int) (vars: VarSet.t) (locations: Location.t list) ranking_template_ template_table_ to_string: unit =
  let execute (i:int) =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = ranking_template_ vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> TemplateTable.add (List.nth !template_table_ i) location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars)-> fresh_vars)
    |> List.flatten
    |> (fun fresh_vars -> fresh_coeffs := fresh_vars)
  in
  for i = !numberOfGeneratedTemplates to depth - 1 do
    Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_mprf_templates_" ^ string_of_int i, [])
      ~result:(fun () ->
          (List.nth !template_table_ i)
          |> TemplateTable.enum
          |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ to_string polynomial)
        )
      (fun () -> execute i);
  done

let compute_ranking_templates cache (depth: int) (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_ depth vars locations ranking_template (get_template_table cache) ParameterPolynomial.to_string

let compute_ranking_templates_real cache (depth: int) (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_ depth vars locations ranking_template_real (get_template_table_real cache) RealParameterPolynomial.to_string

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
let transition_constraint_ (cache, depth, measure, constraint_type, (l,t,l')): Formula.t =
  let res = ref Formula.mk_true in
    for i = 1 to (depth - 1) do
      res := ((List.nth !(get_template_table cache) (i - 1)), (List.nth !(get_template_table cache) i), measure, constraint_type, (l,t,l'))
            |> transition_constraint_i
            |> Formula.mk_and !res
    done;
    res := ((List.nth !(get_template_table cache) 0), measure, constraint_type, (l,t,l'))
          |> transition_constraint_1
          |> Formula.mk_and !res;
    if depth > 1 then
      res := ((List.nth !(get_template_table cache) (depth - 1)), measure, constraint_type, (l,t,l'))
          |> transition_constraint_d ParameterPolynomial.zero
          |> Formula.mk_and !res
    else 
      res := ((List.nth !(get_template_table cache) (depth - 1)), measure, constraint_type, (l,t,l'))
          |> transition_constraint_d ParameterPolynomial.one
          |> Formula.mk_and !res;
    
    !res

let transition_constraint = constraint_cache#add transition_constraint_

let transitions_constraint cache depth measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint (cache, depth, measure, constraint_type, t) )
  |> Formula.all
  
let non_increasing_constraint cache depth measure transition =
  transition_constraint (cache, depth, measure, `Non_Increasing, transition)

let non_increasing_constraints cache depth measure transitions =
  transitions_constraint cache depth measure `Non_Increasing (TransitionSet.to_list transitions)

let decreasing_constraint cache depth measure transition =
  transition_constraint (cache, depth, measure, `Decreasing, transition)

(** A valuation is a function which maps from a finite set of variables to values *)

let rank_from_valuation cache depth (i: int) valuation location =
  location
  |> TemplateTable.find (List.nth !(get_template_table cache) i)
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
    rank = List.init depth (fun i -> rank_from_valuation_real (TemplateTable.find (List.nth !(get_template_table_real cache) i)) valuation);
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
    depth = depth;
  }


(** We are searching for a real model, hence we need to cast reals to integers. *)
let change_valuation (values: RealPolynomial.valuation) = 
  Valuation.from (List.map (fun x -> (x,  MPRF_Invariants.Valuation.eval x values |> OurFloat.upper_int)) (MPRF_Invariants.Valuation.vars values))

let try_decreasing cache program ?(inv = false) depth (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) applied_cfr =
  non_increasing
  |> Stack.enum
  |> Enum.filter (fun t -> not (RankingTable.mem (get_ranking_table measure cache) t))
  |> Enum.iter (fun decreasing ->
        let current_time = Unix.gettimeofday() in
        Logger.(log logger DEBUG (fun () -> "try_decreasing", ["measure", show_measure measure;
                                                                "decreasing", Transition.to_id_string decreasing;
                                                                "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                                                "depth", string_of_int depth]));
        SMTSolverInt.push solver_int;
        if inv then (      
          SMTSolver.push solver_real;
          let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !(get_template_table_real cache) n)) in 
          SMTSolver.add_real solver_real (MPRF_Invariants.decreasing_constraint depth measure decreasing templates_real);
          SMTSolver.add_real solver_real (MPRF_Invariants.consecution_constraint depth measure decreasing templates_real);
          Program.entry_transitions logger program [decreasing]
          |> List.iter (fun trans ->  SMTSolver.add_real solver_real (MPRF_Invariants.initiation_constraint depth measure trans templates_real)));
        
        SMTSolverInt.add solver_int (decreasing_constraint cache depth measure decreasing);
        
        if SMTSolverInt.satisfiable solver_int then (
          SMTSolverInt.minimize_absolute solver_int !fresh_coeffs;
          SMTSolverInt.model solver_int
          |> Option.map (make cache depth decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum))
          |> Option.may (fun ranking_function ->
              to_be_found := !to_be_found - 1;
              RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
              Logger.(log logger INFO (fun () -> "add_mprf", [
                                            "measure", show_measure measure;
                                            "decreasing", Transition.to_id_string decreasing;
                                            "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                            "rank", only_rank_to_string ranking_function])))
        )
        else if inv && SMTSolver.satisfiable solver_real then (
          (* SMTSolver.minimize_absolute solver_real !fresh_coeffs;  Check if minimization is forgotten. *)
          SMTSolver.model_real solver_real
           |> Option.map (
              make_inv cache depth decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum) 
              % tap (fun x -> MPRF_Invariants.store_inv x decreasing)
              % tap (fun x -> MPRF_Invariants.store_inv_set x (non_increasing |> Stack.enum |> TransitionSet.of_enum)))
          |> Option.may (fun ranking_function ->
              to_be_found := !to_be_found - 1;
              RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
              Logger.(log logger INFO (fun () -> "add_mprf_inv", [
                  "measure", show_measure measure;
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


let rec backtrack cache ?(inv = false) depth (steps_left: int) (index: int) (solver_real: SMTSolver.t) (solver_int: SMTSolverInt.t) 
(scc: Transition.t array) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) (program: Program.t) applied_cfr =
    if SMTSolverInt.satisfiable solver_int || (inv && SMTSolver.satisfiable solver_real) then (
      if steps_left == 0 then (
        try_decreasing cache program ~inv:inv depth solver_real solver_int non_increasing to_be_found measure applied_cfr
      ) else (
        for i=index to Array.length scc - 1 do
          let transition = Array.get scc i in
          
          SMTSolverInt.push solver_int;
          if inv then (      
            SMTSolver.push solver_real;
            let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !(get_template_table_real cache) n)) in 
            SMTSolver.add_real solver_real (MPRF_Invariants.non_increasing_constraint depth measure transition templates_real);
            SMTSolver.add_real solver_real (MPRF_Invariants.consecution_constraint depth measure transition templates_real);
            Program.entry_transitions logger program [transition]
            |> List.iter (fun trans ->  SMTSolver.add_real solver_real (MPRF_Invariants.initiation_constraint depth measure trans templates_real)));
          
          SMTSolverInt.add solver_int (non_increasing_constraint cache depth measure transition);
          Stack.push transition non_increasing;
          backtrack cache ~inv:inv depth (steps_left - 1) (i + 1) solver_real solver_int scc non_increasing to_be_found measure program applied_cfr;
          ignore (Stack.pop non_increasing);
          
          SMTSolverInt.pop solver_int;
          if inv then
            SMTSolver.pop solver_real;
        done;
        try_decreasing cache program ~inv:inv depth solver_real solver_int non_increasing to_be_found measure applied_cfr;
      )
    )

let compute_ cache ?(inv = false) measure applied_cfr program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc ->
         try
           for depth = 1 to !maxDepth do
           if !numberOfGeneratedTemplates < depth then (
           compute_ranking_templates cache depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
           if inv then
            compute_ranking_templates_real cache depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
           numberOfGeneratedTemplates := depth);
           backtrack cache
                     ~inv:inv
                     depth 
                     (TransitionSet.cardinal scc)
                     0
                     (SMTSolver.create ())
                     (SMTSolverInt.create ())
                     (Array.of_enum (TransitionSet.enum scc))
                     (Stack.create ())
                     (ref (TransitionSet.cardinal scc))
                     measure
                     program
                     applied_cfr;
          done; 
           scc
           |> TransitionSet.iter (fun t ->
                  if not (RankingTable.mem (get_ranking_table measure cache) t) then
                    Logger.(log logger WARN (fun () -> "no_mprf", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
                ) 
         with Exit -> ()
        )

let compute_scc cache ?(inv = false) measure applied_cfr program scc =
  try
    for depth = 1 to !maxDepth do
    if !numberOfGeneratedTemplates < depth then (
      compute_ranking_templates cache depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if inv then
      compute_ranking_templates_real cache depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    numberOfGeneratedTemplates := depth);
    backtrack cache
              ~inv:inv
              depth 
              (TransitionSet.cardinal scc)
              0
              (SMTSolver.create ())
              (SMTSolverInt.create ())
              (Array.of_enum (TransitionSet.enum scc))
              (Stack.create ())
              (ref (TransitionSet.cardinal scc))
              measure
              program
              applied_cfr;
  done; 
    scc
    |> TransitionSet.iter (fun t ->
          if not (RankingTable.mem (get_ranking_table measure cache) t) then
            Logger.(log logger WARN (fun () -> "no_mprf", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
        ) 
  with Exit -> ()
        
let logging measure transition methode_name = 
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> methode_name, ["measure", show_measure measure;
                                                        "transition", Transition.to_id_string transition])
                  ~result:(Util.enum_to_string to_string % List.enum)

let find cache ?(inv = false) measure applied_cfr program transition =
  let execute () =
    if inv then 
      if MPRF_Invariants.template_table_is_empty then
        MPRF_Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_ cache ~inv:inv measure applied_cfr program;
    (try
       RankingTable.find_all (get_ranking_table measure cache) transition
     with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_mprf" execute

let find_scc cache ?(inv = false) measure applied_cfr program transition scc =
    let execute () =
    if inv then
      if MPRF_Invariants.template_table_is_empty then
        MPRF_Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_scc cache ~inv:inv measure applied_cfr program scc;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_scc_mprf" execute

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
      List.fold_left (fun ps next_ts -> ListMonad.Monad.(next_ts >>= fun t -> List.map (fun p -> p@[t]) ps))
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
          let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !(get_template_table_real cache) n)) in
          SMTSolver.add_real solver_real (MPRF_Invariants.non_increasing_constraint depth measure e templates_real);
          SMTSolver.add_real solver_real (MPRF_Invariants.consecution_constraint depth measure e templates_real);
          Program.entry_transitions logger program [e]
          |> List.iter (fun trans ->  SMTSolver.add_real solver_real (MPRF_Invariants.initiation_constraint depth measure trans templates_real))
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
        let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !(get_template_table_real cache) n)) in
          SMTSolver.add_real solver_real (MPRF_Invariants.non_increasing_constraint depth measure transition templates_real);
          SMTSolver.add_real solver_real (MPRF_Invariants.consecution_constraint depth measure transition templates_real);
          Program.entry_transitions logger program [transition]
          |> List.iter (fun trans ->  SMTSolver.add_real solver_real (MPRF_Invariants.initiation_constraint depth measure trans templates_real))
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

let compute_and_add_ranking_function cache ?(inv = false) applied_cfr program measure all_trans decreasing : unit =  
  let current_time = Unix.gettimeofday () in  
  try
    for current_depth = 1 to !maxDepth do 
      if !numberOfGeneratedTemplates < current_depth then (
        compute_ranking_templates cache current_depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
        if inv then
          compute_ranking_templates_real cache current_depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
        numberOfGeneratedTemplates := current_depth);
    let try_non_inc_set = TransitionSet.remove decreasing all_trans in
    let solver_int = SMTSolverInt.create () in
    let solver_real = SMTSolver.create () in
    if inv then (
      let templates_real = List.init current_depth (fun n -> TemplateTable.find (List.nth !(get_template_table_real cache) n)) in 
      SMTSolver.add_real solver_real (MPRF_Invariants.decreasing_constraint current_depth measure decreasing templates_real);
      SMTSolver.add_real solver_real (MPRF_Invariants.consecution_constraint current_depth measure decreasing templates_real);
      Program.entry_transitions logger program [decreasing]
      |> List.iter (fun trans ->  SMTSolver.add_real solver_real (MPRF_Invariants.initiation_constraint current_depth measure trans templates_real)));
    
    SMTSolverInt.add solver_int (decreasing_constraint cache current_depth measure decreasing);
    if SMTSolverInt.satisfiable solver_int then (
      let non_inc = find_non_inc_set ~inv:false cache current_depth program measure solver_real solver_int decreasing try_non_inc_set in
      SMTSolver.minimize_absolute_old solver_real !fresh_coeffs;
      SMTSolverInt.model solver_int
      |> Option.map (make cache current_depth decreasing non_inc)
      |> Option.may (fun rank_func ->
        RankingTable.add (get_ranking_table measure cache) decreasing rank_func;
        Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                      "measure", show_measure measure;
                                      "decreasing", Transition.to_id_string decreasing;
                                      "non_increasing", TransitionSet.to_string non_inc;
                                      "rank", only_rank_to_string rank_func])));
      raise Exit
    );
    if inv && SMTSolver.satisfiable solver_real then (
      let non_inc = find_non_inc_set ~inv:inv cache current_depth program measure solver_real solver_int decreasing try_non_inc_set in
      (* SMTSolver.minimize_absolute_old solver_real !fresh_coeffs; *)
      if inv then
      SMTSolver.model_real solver_real
        |> Option.map (
          make_inv cache current_depth decreasing non_inc
          % tap (fun x -> MPRF_Invariants.store_inv x decreasing)
          % tap (fun x -> MPRF_Invariants.store_inv_set x non_inc))
      |> Option.may (fun rank_func ->
        RankingTable.add (get_ranking_table measure cache) decreasing rank_func;
        Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                      "measure", show_measure measure;
                                      "decreasing", Transition.to_id_string decreasing;
                                      "non_increasing", TransitionSet.to_string non_inc;
                                      "rank", only_rank_to_string rank_func])));
      raise Exit
    )
      done;
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
    if inv then
      if MPRF_Invariants.template_table_is_empty then
        MPRF_Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);  
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
    if inv then 
      if MPRF_Invariants.template_table_is_empty then
        MPRF_Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_scc_fast cache ~inv:inv measure applied_cfr program scc;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_ranking_functions_scc_fast" execute

(* Useful for testing*)
let reset cache =
  constraint_cache#clear;
  MPRF_Invariants.clear_cache;
  numberOfGeneratedTemplates := 0;
  RankingTable.clear (get_time_ranking_table cache);
  RankingTable.clear (get_cost_ranking_table cache);
  MPRF_Invariants.template_table_clear;
  List.iter (fun e -> TemplateTable.clear e) !(get_template_table cache);
  List.iter (fun e -> TemplateTable.clear e) !(get_template_table_real cache);