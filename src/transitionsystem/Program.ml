open Batteries
open Constraints
open Formulas

open ProgramTypes

module LocationMap = Map.Make (Location)

exception NotWellFormed of string

type t = {
    graph: TransitionGraph.t;
    start: Location.t;
    invariants: Constraint.t LocationMap.t;
  }

let empty_inv_map locations =
  LocationSet.fold (fun loc inv_map -> LocationMap.add loc Constraint.mk_true inv_map) locations (LocationMap.empty)

let equal equal_graph program1 program2 =
  equal_graph program1.graph program2.graph
  && Location.equal program1.start program2.start

let equivalent =
  equal TransitionGraph.equivalent

let transitions t =
  TransitionGraph.transitions t.graph

let generalized_transitions program =
  GeneralTransitionSet.of_transitionset (transitions program)

let locations transitions =
  transitions
  |> Enum.concat_map (fun (l,_,l') -> List.enum [l; l'])
  |> Enum.uniq_by Location.equal

let locations_of_program =
  LocationSet.of_enum % locations % TransitionSet.enum % transitions

(* raises an exception if the program is not well formed *)
let raise_if_not_well_formed t =
  let gts = generalized_transitions t in

  let check_probability () =
    GeneralTransitionSet.iter ( fun gt ->
      if not (OurFloat.(GeneralTransition.total_probability gt = one)) then
        raise  @@ NotWellFormed ("General Transition " ^ GeneralTransition.to_id_string gt
          ^ " has total probability " ^ OurFloat.to_string (GeneralTransition.total_probability gt))
    ) gts
  in

  let check_transition_leading_back () =
    if TransitionSet.exists (Location.equal t.start % Transition.target) (transitions t) then
      raise (Failure "Transition leading back to the initial location.")
  in

  let check_arity () =
    ignore @@ LocationSet.fold
      (fun loc map ->
        let name = Location.name loc in
        let arity = Location.arity loc in
        let prev_arity = Map.find_opt name map in
        match prev_arity with
        | None -> Map.add name arity map
        | Some prev_arity ->
            if not (prev_arity = arity) then (
              raise @@ NotWellFormed ("Location " ^ name ^ " has arities " ^ Int.to_string prev_arity ^ ", and " ^ Int.to_string arity)
            ) else map
      )
      (locations_of_program t) Map.empty
  in

  check_probability ();
  check_transition_leading_back ();
  check_arity ();
  t

let add_locations_to_graph locations graph =
  locations
  |> Enum.map (fun l -> fun gr -> TransitionGraph.add_vertex gr l)
  |> Enum.fold (fun gr adder -> adder gr) graph

let add_location location p =
  {p with invariants = LocationMap.add location Constraint.mk_true p.invariants; graph = TransitionGraph.add_vertex p.graph location}

let add_locations locations p =
  Enum.fold (fun p l -> add_location l p) p locations

let add_transitions transitions graph =
  transitions
  |> Enum.map (fun t -> fun gr -> TransitionGraph.add_edge_e gr t)
  |> Enum.fold (fun gr adder -> adder gr) graph

let remove_location program location =
  { program with graph = TransitionGraph.remove_vertex program.graph location }

let remove_transition program transition =
  { program with graph = TransitionGraph.remove_edge_e program.graph transition }

let map_graph f program =
  { program with graph = f program.graph }

let map_invariants f program =
  { program with invariants = f program.invariants }

let locations transitions =
  transitions
  |> Enum.concat_map (fun (l,_,l') -> List.enum [l; l'])
  |> Enum.uniq_by Location.equal

let mk transitions =
  let locations = locations (Enum.clone transitions) in
  TransitionGraph.empty
  |> add_locations_to_graph locations
  |> add_transitions transitions

let rename program =
  let counter: int ref = ref 0 in
  let map = Hashtbl.create 10 in
  let name location =
    Hashtbl.find_option map location
    |> Option.default_delayed (fun () ->
           let new_name = ("l" ^ string_of_int !counter) in
           Hashtbl.add map location new_name;
           counter := !counter + 1;
           Logger.(log Logging.(get Preprocessor) INFO (fun () -> "renaming", ["original", Location.to_string location; "new", new_name]));
           new_name
         )
    |> fun name -> Location.of_string_and_arity name (Location.arity location)
  in
  let new_start = name program.start in
  let new_graph = TransitionGraph.map_vertex name program.graph in
  {
    graph = new_graph;
    start = new_start;
    invariants = empty_inv_map (TransitionGraph.locations new_graph)
  }

let from transitions start =
    let new_graph = mk (List.enum transitions) in
    {
      graph = new_graph;
      start = start;
      invariants = empty_inv_map (TransitionGraph.locations new_graph)
    }
    |> raise_if_not_well_formed

let from_startstr transitions start =
  let start_trans =
    transitions
    |> List.filter ((=) start % Location.name % Transition.src)
  in
  let start_loc_arity = match start_trans with
    | [] -> 0
    | (t::ts) -> Location.arity (Transition.src t)
  in

  from transitions (Location.of_string_and_arity start start_loc_arity)

let start program = program.start

let graph g = g.graph

let invariant location program = LocationMap.find location (program.invariants)

let vars program =
  program
  |> transitions
  |> TransitionSet.enum
  |> Enum.map Transition.label
  |> Enum.map TransitionLabel.vars
  |> Enum.fold VarSet.union VarSet.empty

let input_vars program =
  program
  |> transitions
  |> TransitionSet.enum
  |> Enum.map Transition.label
  |> Enum.map TransitionLabel.input_vars
  |> Enum.fold VarSet.union VarSet.empty

let temp_vars =
  fun program -> VarSet.diff (vars program) (input_vars program)

let pre program (l,t,_) =
  l
  |> TransitionGraph.pred_e (graph program)
  |> List.enum
  |> Enum.filter (fun (_,t',_) ->
         TransitionLabel.append_guard t' t
         |> Formula.mk
         |> SMT.Z3Solver.satisfiable_int
       )

let pre_gt program gt =
  let gts = transitions program |> GeneralTransitionSet.of_transitionset in
  let pre_ts = pre program (GeneralTransition.transitions gt |> TransitionSet.any) |> TransitionSet.of_enum in
  gts
  |> GeneralTransitionSet.filter (TransitionSet.exists (fun t -> TransitionSet.mem t pre_ts) % GeneralTransition.transitions)

let sccs_locs program =
  let module SCC = Graph.Components.Make(TransitionGraph) in
  program.graph
  |> SCC.scc_list
  |> List.rev

let all_sccs_locs program =
  let module SCC = Graph.Components.Make(TransitionGraph) in

  let is_scc lset =
    let gr = graph program in
    let gr' =
      TransitionGraph.fold_edges_e
      (fun (l,t,l') gr -> if LocationSet.mem l lset && LocationSet.mem l' lset then gr else TransitionGraph.remove_edge_e gr (l,t,l')) gr gr
      |> TransitionGraph.fold_vertex (fun l gr -> if not (LocationSet.mem l lset) then TransitionGraph.remove_vertex gr l else gr) gr
    in
    SCC.scc_list gr'
    |> List.enum
    |> Enum.map List.enum
    |> List.of_enum
    |> Int.equal 1 % List.length
  in

  sccs_locs program
  |> List.enum
  |> Enum.map LocationSet.of_list
  |> Enum.map (LocationSet.powerset)
  |> Enum.flatten
  |> Enum.filter is_scc
  |> Enum.map (LocationSet.to_list)

let sccs program =
  sccs_locs program
  |> List.enum
  |> Enum.map (TransitionGraph.loc_transitions program.graph)
  |> Enum.filter (not % TransitionSet.is_empty)

let all_sccs program =
  all_sccs_locs program
  |> Enum.map (TransitionGraph.loc_transitions program.graph)
  |> Enum.filter (not % TransitionSet.is_empty)

let non_trivial_transitions =
  Enum.fold TransitionSet.union TransitionSet.empty % sccs

let add_invariant location invariant program =
  map_graph (TransitionGraph.add_invariant location invariant) program
  |> map_invariants (LocationMap.modify location (Constraint.mk_and invariant))

let is_initial program trans =
  Location.(equal (program.start) (Transition.src trans))

let is_initial_gt program trans =
  Location.(equal (program.start) (GeneralTransition.start trans))

let is_initial_location program location =
  Location.(equal (program.start) location)

let to_formatted_string program =
  let gts = GeneralTransitionSet.of_transitionset (transitions program) in
  let transitions =
    GeneralTransitionSet.fold (fun gt str -> str @ [GeneralTransition.to_string gt]) gts []
    |> List.map FormattedString.mk_str_line
    |> FormattedString.mappend
  in
  let locations = String.concat ", " (TransitionGraph.fold_vertex (fun l str -> str @ [(Location.to_string l)]) program.graph []) in
  FormattedString.format_append
    ([
        "  Start:" ^ Location.to_string program.start;
        "Program_Vars:" ^ (program |> input_vars |> VarSet.map_to_list Var.to_string |> String.concat ", ");
        "Temp_Vars:" ^ (program |> temp_vars |> VarSet.map_to_list Var.to_string |> String.concat ", ");
        "Locations:" ^ locations;
        "Transitions:"
      ]
    |> List.map FormattedString.mk_str_line |> FormattedString.mappend)
    transitions

let to_string program =
  FormattedString.render_string @@ to_formatted_string program

let to_simple_string ~show_gtcost program =
  TransitionGraph.fold_edges_e (fun t str -> str ^ ", " ^ Transition.to_string ~show_gtcost:show_gtcost t) program.graph ""

let test program trans g_set =
  GeneralTransitionSet.add (GeneralTransition.of_transitionset (program |> transitions) trans) g_set
