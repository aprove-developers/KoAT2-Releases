open Batteries
open Constraints
   
open ProgramTypes
open RVGTypes
   
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
  
let add_vertices_to_rvg vertices rvg =
  vertices
  |> List.map (flip RVG.add_vertex)
  |> List.fold_left (fun rvg adder -> adder rvg) rvg

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

let pre program (l,t,l') =
  List.enum (TransitionGraph.pred_e (graph program) l)

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

let rvg kind program =
  let add_transition (post_transition: Transition.t) (rvg: RVG.t): RVG.t =
    let rvg_with_vertices: RVG.t = add_vertices_to_rvg (program |> vars |> VarSet.to_list |> List.map (fun var -> (post_transition,var))) rvg in
    let pre_nodes (post_transition: Transition.t) (post_var: Var.t) =
      let vars =
        LocalSizeBound.sizebound_local kind (vars program) post_transition post_var
        |> Option.map LocalSizeBound.vars
        |? vars program
      in
      vars
      |> VarSet.enum
      |> Enum.cartesian_product (pre program post_transition)
      |> Enum.map (fun (pre_transition,pre_var) -> (pre_transition,pre_var,post_var))
    in
    vars program
    |> VarSet.enum
    |> Enum.map (pre_nodes post_transition)
    |> Enum.flatten
    |> Enum.fold (fun rvg (pre_transition,pre_var,post_var) -> RVG.add_edge rvg (pre_transition,pre_var) (post_transition,post_var)) rvg_with_vertices
  in
  TransitionGraph.fold_edges_e add_transition program.graph RVG.empty
  
let print_graph out_dir name graph output_graph =
  let full_path ext =
    Fpath.(to_string (out_dir // (v name |> add_ext ext)))
  in
  (* Create output directory if not existing *)
  ignore (Sys.command ("mkdir -p " ^ Fpath.to_string out_dir));
  (* Write a graphviz dot file *)
  output_graph (Pervasives.open_out_bin (full_path "dot")) graph;
  (* Generate a png from the dot file with an external call to graphviz *)
  ignore (Sys.command ("dot -T png -o " ^ full_path "png" ^ " " ^ full_path "dot"))
  
let print_system ~label ~outdir ~file program =
  (* Definition of some graphviz options how it should be layout *)
  let module Dot = Graph.Graphviz.Dot(struct
                                       include TransitionGraph
                                       let edge_attributes (a, e, b) = [`Label (label e); `Color 4711]
                                       let default_edge_attributes _ = []
                                       let get_subgraph _ = None
                                       let vertex_attributes _ = [`Shape `Box]
                                       let vertex_name v = Location.to_string v
                                       let default_vertex_attributes _ = []
                                       let graph_attributes _ = []
                                     end) in
  print_graph outdir (file ^ "_system") (graph program) Dot.output_graph

let print_rvg kind ~label ~outdir ~file program =
  let graph = rvg kind program in
  let module C = Graph.Components.Make(RVG) in
  let (_,scc_number) = C.scc graph in
  let rv_color (rv: RV.t) =
    scc_number rv * 424242
  in
  let show_kind = function
    | `Lower -> "lower"
    | `Upper -> "upper"
  in
  (* Definition of some graphviz options how it should be layout *)
  let module Dot = Graph.Graphviz.Dot(struct
                                       include RVG
                                       let edge_attributes _ = [`Label ""; `Color 4711]
                                       let default_edge_attributes _ = []
                                       let get_subgraph _ = None
                                       let vertex_attributes v = [`Shape `Box; `Color (rv_color v)]
                                       let vertex_name v = "\"" ^ label v ^ "\""
                                       let default_vertex_attributes _ = []
                                       let graph_attributes _ = []
                                     end) in
  print_graph outdir (file ^ "_rvg_" ^ show_kind kind) graph Dot.output_graph

let is_initial program trans =
  Location.(equal (program.start) (Transition.src trans))

let is_initial_location program location =
  Location.(equal (program.start) location)

let to_string program =
  let transitions = TransitionGraph.fold_edges_e (fun t str -> str ^ "; " ^ Transition.to_string t) program.graph ""
  and locations = TransitionGraph.fold_vertex (fun l str -> str ^ "; " ^ Location.to_string l) program.graph "" in
  String.concat " " [
      "Start:"; Location.to_string program.start;
      "Locations:"; locations;
      "Transitions:"; transitions;
      "Vars:"; program |> vars |> VarSet.map_to_list Var.to_string |> String.concat ", "
    ] 
  
let to_simple_string program =
  TransitionGraph.fold_edges_e (fun t str -> str ^ ", " ^ Transition.to_string t) program.graph "" 
