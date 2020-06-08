open Batteries
open ProgramTypes
open RVGTypes
   
let print_graph out_dir name graph output_graph =
  let full_path ext =
    Fpath.(to_string (out_dir // (v name |> add_ext ext)))
  in
  (* Create output directory if not existing *)
  ignore (Sys.command ("mkdir -p " ^ Fpath.to_string out_dir));
  (* Write a graphviz dot file *)
  output_graph (Stdlib.open_out_bin (full_path "dot")) graph;
  (* Generate a png from the dot file with an external call to graphviz *)
  ignore (Sys.command ("dot -T png -o " ^ full_path "png" ^ " " ^ full_path "dot"))
  
(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
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
  print_graph outdir (file ^ "_system") (Program.graph program) Dot.output_graph

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the result variable graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
let print_rvg kind ~label ~outdir ~file program =
  let graph = RVG.rvg kind program in
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
    
