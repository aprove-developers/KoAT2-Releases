open Batteries

module Location =
  struct
    type t = {
        name : string;
        (*initial : bool;*)
        (* TODO Possible optimization: invariant : PolynomialConstraints.t*)
      } [@@deriving eq, ord]
           
    let to_string l = l.name
                    
    let hash l = Hashtbl.hash (to_string l)
               
    let of_string inp_name = { name = inp_name }               
  end
module LocationSet = Set.Make(Location)

module Transition =
  struct
    type t = Location.t * TransitionLabel.t * Location.t [@@deriving eq, ord]

    let src (src, _, _) = src
           
    let label (_, label, _) = label

    let target (_, _, target) = target

    let cost t = TransitionLabel.cost (label t)

    (* Needs to be fast for usage in the timebound hashtables.
       There might be transitions with the same src and target, 
       but that is not a problem for the hashtables,
       since it should not occur very often. *)
    let hash (l,_,l') = Hashtbl.hash (Location.to_string l ^ Location.to_string l')

    let to_src_target_string (l,_,l') =
      Location.to_string l ^ "->" ^ Location.to_string l'

    let to_string (l,t,l') =
       to_src_target_string (l,t,l') ^ ", " ^ TransitionLabel.to_string t
  end
module TransitionSet = Set.Make(Transition)

module TransitionGraph =
  struct
    include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(TransitionLabel)

    let locations graph =
      fold_vertex LocationSet.add graph LocationSet.empty

    let transitions graph =
      fold_edges_e TransitionSet.add graph TransitionSet.empty

    let equal graph1 graph2 =
      LocationSet.equal (locations graph1) (locations graph2)
      && TransitionSet.equal (transitions graph1) (transitions graph2)

  end

module RV =
  struct
    type t = Transition.t * Var.t [@@deriving eq, ord]
    let hash (t,v) = Hashtbl.hash (Transition.to_string t ^ Var.to_string v)
    let transition (t,v) = t
    let variable (t,v) = v
    let to_string ((l,t,l'),v) = Bound.to_string (LocalSizeBound.(as_bound (sizebound_local `Upper t v))) ^ " >= " ^
                                   "|" ^ Location.to_string l ^ " -> " ^ Location.to_string l' ^ "," ^ Var.to_string v ^ "|"
                                   ^ " >= " ^ Bound.to_string (LocalSizeBound.(as_bound (sizebound_local `Lower t v)))
  end
module RVG =
  struct
    include Graph.Persistent.Digraph.ConcreteBidirectional(RV)

    type scc = RV.t Enum.t

    let pre rvg rv =
      pred rvg rv
      |> List.enum

    (* TODO Optimizable *)
    let entry_points rvg scc =
      scc
      |> Enum.map (pre rvg)
      |> Enum.flatten
      |> Enum.uniq_by RV.equal
      |> Util.intersection RV.equal scc

    let transitions scc =
      scc
      |> Enum.map RV.transition
      |> Enum.uniq_by Transition.equal
  end  
   
type t = {
    graph: TransitionGraph.t;
    vars: Var.t list;
    start: Location.t;
  } [@@deriving eq]
       
let add_vertices graph vertices =
  vertices
  |> Enum.map (fun vertex -> fun gr -> TransitionGraph.add_vertex gr vertex)
  |> Enum.fold (fun gr adder -> adder gr) graph
  
let add_edges graph edges =
  edges
  |> Enum.map (fun edge -> fun gr -> TransitionGraph.add_edge_e gr edge)
  |> Enum.fold (fun gr adder -> adder gr) graph

let remove_location program location =
  { program with graph = TransitionGraph.remove_vertex program.graph location }

let remove_transition program transition =
  { program with graph = TransitionGraph.remove_edge_e program.graph transition }

let map_graph f program =
  { program with graph = f program.graph }

let mk vertices edges =
  add_edges (add_vertices TransitionGraph.empty vertices) edges

let from vars transitions start =
  let edges = Enum.map (fun t -> (Location.of_string (TransitionLabel.start t),
                                  t,
                                  Location.of_string (TransitionLabel.target t)))
                       (List.enum transitions) in
  let vertices =
    Enum.clone edges
    |> Enum.concat_map (fun (l,_,l') -> List.enum [l; l'])
    |> Enum.uniq in
  {
    graph = mk vertices edges;
    vars = vars;
    start = start;
  }

let vars program =
  VarSet.of_list program.vars
  
let start program = program.start
  
let add_vertices_to_rvg vertices rvg =
  vertices
  |> List.map (flip RVG.add_vertex)
  |> List.fold_left (fun rvg adder -> adder rvg) rvg

let graph g = g.graph
            
let pre program (l,t,l') =
  List.enum (TransitionGraph.pred_e (graph program) l)

let rvg program =
  let add_transition (post_transition: Transition.t) (rvg: RVG.t): RVG.t =
    let rvg_with_vertices: RVG.t = add_vertices_to_rvg (List.map (fun var -> (post_transition,var)) program.vars) rvg in
    let pre_nodes (post_transition: Transition.t) (post_var: Var.t) =
      LocalSizeBound.sizebound_local `Upper (Transition.label post_transition) post_var
      |> LocalSizeBound.as_bound
      |> Bound.vars
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
  
let print_system ~outdir ~file program =
  (* Definition of some graphviz options how it should be layout *)
  let module Dot = Graph.Graphviz.Dot(struct
                                       include TransitionGraph
                                       let edge_attributes (a, e, b) = [`Label (TransitionLabel.to_string e); `Color 4711]
                                       let default_edge_attributes _ = []
                                       let get_subgraph _ = None
                                       let vertex_attributes _ = [`Shape `Box]
                                       let vertex_name v = Location.to_string v
                                       let default_vertex_attributes _ = []
                                       let graph_attributes _ = []
                                     end) in
  print_graph outdir (file ^ "_system") (graph program) Dot.output_graph

let print_rvg ~outdir ~file program =
  (* Definition of some graphviz options how it should be layout *)
  let module Dot = Graph.Graphviz.Dot(struct
                                       include RVG
                                       let edge_attributes _ = [`Label ""; `Color 4711]
                                       let default_edge_attributes _ = []
                                       let get_subgraph _ = None
                                       let vertex_attributes _ = [`Shape `Box]
                                       let vertex_name v = "\"" ^ RV.to_string v ^ "\""
                                       let default_vertex_attributes _ = []
                                       let graph_attributes _ = []
                                     end) in
  print_graph outdir (file ^ "_rvg") (rvg program) Dot.output_graph

let is_initial program trans =
  Location.(equal (program.start) (Transition.src trans))

let is_initial_location program location =
  Location.(equal (program.start) location)

let to_string program =
  let transitions = TransitionGraph.fold_edges_e (fun t str -> str ^ "; " ^ Transition.to_string t) program.graph ""
  and locations = TransitionGraph.fold_vertex (fun l str -> str ^ "; " ^ Location.to_string l) program.graph "" in
  String.concat " " ["Start:"; Location.to_string program.start; "Locations:"; locations; "Transitions:"; transitions; "Vars:"; String.concat ", " (List.map Var.to_string program.vars)] 
