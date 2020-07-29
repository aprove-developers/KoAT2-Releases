open Batteries
open Constraints
open Formulas
   
open ProgramTypes
   
type t = {
    graph: TransitionGraph.t;
    start: Location.t;
  }

let equal equal_graph program1 program2 =
  equal_graph program1.graph program2.graph
  && Location.equal program1.start program2.start

let equivalent =
  equal TransitionGraph.equivalent

let add_locations locations graph =
  locations
  |> Enum.map (fun l -> fun gr -> TransitionGraph.add_vertex gr l)
  |> Enum.fold (fun gr adder -> adder gr) graph
  
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

let locations transitions =
  transitions
  |> Enum.concat_map (fun (l,_,l') -> List.enum [l; l'])
  |> Enum.uniq_by Location.equal
  
let mk transitions =
  let locations = locations (Enum.clone transitions) in
  TransitionGraph.empty
  |> add_locations locations
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
    |> Location.of_string
  in
  let new_start = name program.start in
  {
    graph = TransitionGraph.map_vertex name program.graph;
    start = new_start;
  }

let from transitions start =
  transitions
  |> fun transitions ->
     if transitions |> List.map Transition.target |> List.mem_cmp Location.compare start then
       raise (Failure "Transition leading back to the initial location.")
     else
       {
         graph = mk (List.enum transitions);
         start = start;
       }

let start program = program.start
  
let graph g = g.graph
            
let transitions =
  TransitionGraph.transitions % graph
  
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
         TransitionLabel.append t' t
         |> TransitionLabel.guard
         |> Formula.mk
         |> SMT.Z3Solver.satisfiable
       )

let sccs program =
  let module SCC = Graph.Components.Make(TransitionGraph) in
  program.graph
  |> SCC.scc_list
  |> List.rev
  |> List.enum  
  |> Enum.map (TransitionGraph.loc_transitions program.graph)
  |> Enum.filter (not % TransitionSet.is_empty)
  
let non_trivial_transitions =
  Enum.fold TransitionSet.union TransitionSet.empty % sccs

let add_invariant location invariant =
  map_graph (TransitionGraph.add_invariant location invariant)

let is_initial program trans =
  Location.(equal (program.start) (Transition.src trans))

let is_initial_location program location =
  Location.(equal (program.start) location)

let to_string ?(html=false) program =
  let sep = if html then "<br>" else "\n" in
  let transitions = String.concat (sep ^ "  ") (TransitionGraph.fold_edges_e (fun t str -> str @ [(Transition.to_string t)]) program.graph [])
  and locations = String.concat ", " (TransitionGraph.fold_vertex (fun l str -> str @ [(Location.to_string l)]) program.graph []) in
  String.concat "  " [
      "  Start:"; Location.to_string program.start;sep;
      "Program_Vars:"; program |> input_vars |> VarSet.map_to_list Var.to_string |> String.concat ", "; sep;
      "Temp_Vars:"; program |> temp_vars |> VarSet.map_to_list Var.to_string |> String.concat ", "; sep; 
      "Locations:"; locations; sep;
      ("Transitions:" ^ sep); transitions; sep;
    ] 
  
let to_simple_string program =
  TransitionGraph.fold_edges_e (fun t str -> str ^ ", " ^ Transition.to_string t) program.graph "" 
