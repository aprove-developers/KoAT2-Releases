open Batteries
open Constraints
open Formulas

open ProgramTypes

module LocationMap = Map.Make (Location)

type t = {
    graph: TransitionGraph.t;
    (* Variables as ordered in the original input file*)
    program_vars_ordered: Var.t list;
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

let rename_program_vars rename_map program =
  let rename_graph graph =
    let transitions = (TransitionSet.enum % TransitionGraph.transitions) graph in
    Enum.fold
      (fun program_graph transition -> TransitionGraph.replace_edge_e transition (Transition.rename rename_map transition) program_graph)
      graph transitions
  in
  {
    graph = rename_graph program.graph;
    program_vars_ordered = List.map (fun v -> RenameMap.find v rename_map ~default:v) program.program_vars_ordered;
    start = program.start;
    invariants = program.invariants;
  }
  |> tap (fun p -> Printf.printf "vars %s" @@ (VarSet.to_string % VarSet.of_list @@ p.program_vars_ordered))


let rename_locations program =
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
    |> Location.of_string
  in
  let new_start = name program.start in
  let new_graph = TransitionGraph.map_vertex name program.graph in
  {
    graph = new_graph;
    program_vars_ordered = program.program_vars_ordered;
    start = new_start;
    invariants = empty_inv_map (TransitionGraph.locations new_graph)
  }

let from transitions start program_vars_ordered =
  transitions
  |> fun transitions ->
     if transitions |> List.map Transition.target |> List.mem_cmp Location.compare start then
       raise (Failure "Transition leading back to the initial location.")
     else
       let new_graph = mk (List.enum transitions) in
       {
         graph = new_graph;
         program_vars_ordered;
         start = start;
         invariants = empty_inv_map (TransitionGraph.locations new_graph)
       }

let start program = program.start

let graph g = g.graph

let invariant location program = LocationMap.find location (program.invariants)

let transitions =
  TransitionGraph.transitions % graph

let program_vars_ordered program =
  program.program_vars_ordered

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
         |> SMT.Z3Opt.satisfiable
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

let to_formatted_string ~show_gtcost program =
  let transitions =
    TransitionGraph.fold_edges_e (fun t str -> str @ [(Transition.to_string ~show_gtcost:show_gtcost t)]) program.graph []
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

let to_string ~show_gtcost program =
  FormattedString.render_string @@ to_formatted_string ~show_gtcost:show_gtcost program

let to_simple_string ~show_gtcost program =
  TransitionGraph.fold_edges_e (fun t str -> str ^ ", " ^ Transition.to_string ~show_gtcost:show_gtcost t) program.graph ""

let test program trans g_set =
  GeneralTransitionSet.add (GeneralTransition.of_transitionset (program |> transitions) trans) g_set

let generalized_transitions program =
  GeneralTransitionSet.of_transitionset (transitions program)
