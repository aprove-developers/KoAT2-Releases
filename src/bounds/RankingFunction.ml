open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open CFR 
   
module SMTSolver = SMT.IncrementalZ3Solver
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

type trans_constraint_cache = < add : (measure * constraint_type * ProgramTypes.Transition.t -> Formula.t) -> measure * constraint_type * ProgramTypes.Transition.t -> Formula.t; clear : unit >
type ranking_cache = (t RankingTable.t * t RankingTable.t * Polynomials.ParameterPolynomial.t TemplateTable.t * RealParameterPolynomial.t TemplateTable.t * trans_constraint_cache)
let new_cache: unit -> ranking_cache =
  fun () -> (RankingTable.create 10, RankingTable.create 10, TemplateTable.create 10, TemplateTable.create 10,  Util.cache ~extractor:(Tuple3.map3 Transition.id))

let get_time_ranking_table     (cache: ranking_cache) = Tuple5.first cache
let get_cost_ranking_table     (cache: ranking_cache) = Tuple5.second cache
let get_template_table         (cache: ranking_cache) = Tuple5.third cache
let get_template_table_real    (cache: ranking_cache) = Tuple5.fourth cache
let get_trans_constraint_cache (cache: ranking_cache) = Tuple5.fifth cache

let get_ranking_table measure = 
  match measure with
  | `Time -> get_time_ranking_table 
  | `Cost -> get_cost_ranking_table

let one = ParameterPolynomial.one
        
let logger = Logging.(get PRF)  

let rank f = f.rank
           
let decreasing f = f.decreasing
                 
let non_increasing f = TransitionSet.to_list f.non_increasing

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

let fresh_coeffs: Var.t list ref = ref []

let compute_ranking_templates_ (vars: VarSet.t) (locations: Location.t list) ranking_template_ template_table_ to_string: unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = ranking_template_ vars in
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

let compute_ranking_templates cache (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_ vars locations ranking_template (get_template_table cache) ParameterPolynomial.to_string

let compute_ranking_templates_real cache (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_ vars locations ranking_template_real (get_template_table_real cache) RealParameterPolynomial.to_string

let decreaser measure t =
  match measure with
  | `Cost -> TransitionLabel.cost t
  | `Time -> Polynomial.one

let transition_constraint_ cache (measure, constraint_type, (l,t,l')): Formula.t =
  let template = TemplateTable.find (get_template_table cache) in
  let atom =
    match constraint_type with
    | `Non_Increasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.substitute_f (as_parapoly t) (template l'))
    | `Decreasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.(of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template l')))
    | `Bounded -> ParameterAtom.Infix.(template l >= ParameterPolynomial.of_polynomial (decreaser measure t))      
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk  
       
let transition_constraint cache = (get_trans_constraint_cache cache)#add (transition_constraint_ cache)
  
let transitions_constraint cache measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint cache (measure, constraint_type, t))
  |> Formula.all
  
let non_increasing_constraint cache measure transition =
  transition_constraint cache (measure, `Non_Increasing, transition)

let non_increasing_constraints cache measure transitions =
  transitions_constraint measure cache `Non_Increasing (TransitionSet.to_list transitions)
  
let bounded_constraint cache measure transition =
  transition_constraint cache (measure, `Bounded, transition)

let decreasing_constraint cache measure transition =
  transition_constraint cache (measure, `Decreasing, transition)
  
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

let try_decreasing cache ?(inv = false) (opt: SMTSolver.t) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) applied_cfr =
  non_increasing
  |> Stack.enum
  |> Enum.filter (fun t -> not (RankingTable.mem (get_ranking_table measure cache) t))
  |> Enum.iter (fun decreasing ->
         let current_time = Unix.gettimeofday() in
         Logger.(log logger DEBUG (fun () -> "try_decreasing", ["measure", show_measure measure;
                                                                "decreasing", Transition.to_id_string decreasing;
                                                                "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing)]));
         SMTSolver.push opt;
         if inv then (
          let template = TemplateTable.find (get_template_table_real cache) in
          SMTSolver.add_real opt (Invariants.bounded_constraint measure decreasing template);
          SMTSolver.add_real opt (Invariants.decreasing_constraint measure decreasing template);
          SMTSolver.add_real opt (Invariants.consecution_constraint measure decreasing template);)
         else (
          SMTSolver.add opt (bounded_constraint cache measure decreasing);
          SMTSolver.add opt (decreasing_constraint cache measure decreasing);
         );
         if SMTSolver.satisfiable opt then ( 
           (* SMTSolver.minimize_absolute opt !fresh_coeffs;   *)
           (* SMTSolver.minimize_absolute opt !fresh_coeffs; Check if minimization is forgotten. TODO *)
           SMTSolver.model_real opt
           |> Option.map (
              if inv then (
               make_inv cache decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum) % change_valuation
               % tap (fun x -> Invariants.store_inv x decreasing)
               % tap (fun x -> Invariants.store_inv_set x (non_increasing |> Stack.enum |> TransitionSet.of_enum)))
              else 
               make cache decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum) % change_valuation)
           |> Option.may (fun ranking_function ->
                  to_be_found := !to_be_found - 1;
                  RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
                  Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                               "measure", show_measure measure;
                                               "decreasing", Transition.to_id_string decreasing;
                                               "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                               "rank", only_rank_to_string ranking_function]))
                )
         );
         SMTSolver.pop opt;
          if applied_cfr then (
            CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.gettimeofday() -. current_time);
            CFR.poll_timeout ~applied_cfr:applied_cfr)
       );
  if !to_be_found <= 0 then
    raise Exit

           
let rec backtrack cache ?(inv = false) (steps_left: int) (index: int) (opt: SMTSolver.t) (scc: Transition.t array) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) (program: Program.t) applied_cfr =
  if SMTSolver.satisfiable opt then (
    if steps_left == 0 then (
      try_decreasing cache ~inv:inv opt non_increasing to_be_found measure applied_cfr
    ) else (
      for i=index to Array.length scc - 1 do
        let transition = Array.get scc i in
        SMTSolver.push opt;
        if inv then (
          let template_real = TemplateTable.find (get_template_table_real cache) in
          SMTSolver.add_real opt (Invariants.non_increasing_constraint measure transition template_real);
          SMTSolver.add_real opt (Invariants.consecution_constraint measure transition template_real);
          Program.entry_transitions logger program [transition]
          |> List.iter (fun trans ->  SMTSolver.add_real opt (Invariants.initiation_constraint measure trans template_real));)
        else 
           SMTSolver.add opt (non_increasing_constraint cache measure transition);
        Stack.push transition non_increasing;
        backtrack cache ~inv:inv (steps_left - 1) (i + 1) opt scc non_increasing to_be_found measure program applied_cfr; 
        ignore (Stack.pop non_increasing);
        SMTSolver.pop opt;
      done;
      try_decreasing cache ~inv:inv opt non_increasing to_be_found measure applied_cfr
    )
  )

let compute_ ?(inv = false) cache measure applied_cfr program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc ->
         try
           backtrack cache
                     (TransitionSet.cardinal scc)
                     0
                     (SMTSolver.create ())
                     (Array.of_enum (TransitionSet.enum scc))
                     (Stack.create ())
                     (ref (TransitionSet.cardinal scc))
                     measure
                     program
                     applied_cfr
                     ~inv:inv;
           scc
           |> TransitionSet.iter (fun t ->
                  if not (RankingTable.mem (get_ranking_table measure cache) t) then
                    Logger.(log logger WARN (fun () -> "no_ranking_function", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
                )
         with Exit -> ()
       )

let compute_scc ?(inv = false) cache measure applied_cfr program scc =
  try
    backtrack cache
              (TransitionSet.cardinal scc)
              0
              (SMTSolver.create ())
              (Array.of_enum (TransitionSet.enum scc))
              (Stack.create ())
              (ref (TransitionSet.cardinal scc))
              measure
              program
              applied_cfr
              ~inv:inv;
    scc
    |> TransitionSet.iter (fun t ->
          if not (RankingTable.mem (get_ranking_table measure cache) t) then
            Logger.(log logger WARN (fun () -> "no_ranking_function", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
        )
  with Exit -> ()

let check_cache cache ?(inv = false) measure program applied_cfr = 
  if inv then (
    if Invariants.TemplateTable.is_empty Invariants.template_table_inv then
      Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list););
    if TemplateTable.is_empty (get_template_table cache) then
      compute_ranking_templates cache (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);      
    if TemplateTable.is_empty (get_template_table_real cache) && inv then
      compute_ranking_templates_real cache (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list)

let logging measure transition methode_name = 
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> methode_name, ["measure", show_measure measure;
                                                        "transition", Transition.to_id_string transition])
                  ~result:(Util.enum_to_string to_string % List.enum)

let find cache ?(inv = false) measure applied_cfr program transition =
  let execute () =
    check_cache cache ~inv:inv measure program applied_cfr;
    if RankingTable.is_empty (get_ranking_table measure cache) then 
      compute_ cache measure applied_cfr program ~inv:inv;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_ranking_functions" execute

let find_scc cache ?(inv = false) measure applied_cfr program transition scc =
  let execute () =
      check_cache cache ~inv:inv measure program applied_cfr;
      if RankingTable.is_empty (get_ranking_table measure cache) then
        compute_scc cache measure applied_cfr program ~inv:inv scc;
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

let close_next_cycle cache ?(inv = false) program measure solver not_added_graph graph: TransitionSet.t =
  let all_paths = next_candidates not_added_graph graph in
  let rec close_circle path: bool * TransitionSet.t = match path with
    | []    -> true, TransitionSet.empty
    | e::es ->
        SMTSolver.push solver;
      if inv then 
        let template_real = TemplateTable.find (get_template_table_real cache) in
        SMTSolver.add_real solver (Invariants.non_increasing_constraint measure e template_real);
        SMTSolver.add_real solver (Invariants.consecution_constraint measure e template_real);
        Program.entry_transitions logger program [e]
        |> List.iter (fun trans ->  SMTSolver.add_real solver (Invariants.initiation_constraint measure trans template_real));
      else 
        SMTSolver.add solver (non_increasing_constraint cache measure e);
      if SMTSolver.satisfiable solver then
          let (closed, non_inc_set) = close_circle es in
          SMTSolver.pop solver;
          closed, TransitionSet.add e non_inc_set
        else
          (SMTSolver.pop solver; false, TransitionSet.empty)
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
        SMTSolver.add_real solver (Invariants.non_increasing_constraint measure transition template_real);
        SMTSolver.add_real solver (Invariants.consecution_constraint measure transition template_real);
        Program.entry_transitions logger program [transition]
        |> List.iter (fun trans ->  SMTSolver.add_real solver (Invariants.initiation_constraint measure trans template_real));
      else 
        SMTSolver.add solver (non_increasing_constraint cache measure transition);)
      )


let find_non_inc_set cache ?(inv = false) program measure solver decreasing try_non_inc_set =
  let decr_graph =
    TransitionGraph.empty
    |> flip TransitionGraph.add_edge_e decreasing
  in
  let not_added_graph =
    TransitionGraph.empty
    |> TransitionSet.fold (flip TransitionGraph.add_edge_e) try_non_inc_set
  in
  let rec try_close_cycles n_a_g g =
    let next_non_inc_set = close_next_cycle ~inv:inv cache program measure solver n_a_g g in
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
  let try_non_inc_set = TransitionSet.remove decreasing all_trans in
  let solver = SMTSolver.create () in
  if inv then (
    let template = TemplateTable.find (get_template_table_real cache) in
    SMTSolver.add_real solver (Invariants.bounded_constraint measure decreasing template);
    SMTSolver.add_real solver (Invariants.decreasing_constraint measure decreasing template);
    SMTSolver.add_real solver (Invariants.consecution_constraint measure decreasing template);)
    else (
    SMTSolver.add solver (bounded_constraint cache measure decreasing);
    SMTSolver.add solver (decreasing_constraint cache measure decreasing););
  if SMTSolver.satisfiable solver then (
    let non_inc = find_non_inc_set ~inv:inv cache program measure solver decreasing try_non_inc_set in
    (* SMTSolver.minimize_absolute_old solver !fresh_coeffs; *)
    SMTSolver.model_real solver
    |> Option.map (
      if inv then (
        make_inv cache decreasing non_inc % change_valuation
        % tap (fun x -> Invariants.store_inv x decreasing)
        % tap (fun x -> Invariants.store_inv_set x non_inc))
      else 
        make cache decreasing non_inc % change_valuation)
    |> Option.may (fun rank_func ->
      RankingTable.add (get_ranking_table measure cache) decreasing rank_func;
      Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                    "measure", show_measure measure;
                                    "decreasing", Transition.to_id_string decreasing;
                                    "non_increasing", TransitionSet.to_string non_inc;
                                    "rank", only_rank_to_string rank_func]))))
    |> tap (fun _ ->          
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
    check_cache cache ~inv:inv measure program applied_cfr;
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
    check_cache cache ~inv:inv measure program applied_cfr;
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_scc_fast cache ~inv:inv measure applied_cfr program scc;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  logging measure transition "find_ranking_functions_scc_fast" execute

let reset cache =
  (get_trans_constraint_cache cache)#clear;
  Invariants.cache#clear;
  RankingTable.clear (get_time_ranking_table cache);
  RankingTable.clear (get_cost_ranking_table cache);
  Invariants.TemplateTable.clear Invariants.template_table_inv;
  TemplateTable.clear (get_template_table cache);
  TemplateTable.clear (get_template_table_real cache)