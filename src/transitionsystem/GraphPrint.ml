open Batteries
open ProgramTypes
open RVGTypes

module RV = RVG.RV
module ERV = Make_RV(RVTransitions.TransitionForExpectedSize)
module VarMap = TransitionLabel.VarMap

let print_graph ?(format="png") out_dir name graph output_graph =
  let full_path ext =
    Fpath.(to_string (out_dir // (v name |> add_ext ext)))
  in
  (* Create output directory if not existing *)
  ignore (Sys.command ("mkdir -p " ^ Fpath.to_string out_dir));
  (* Write a graphviz dot file *)
  output_graph (Stdlib.open_out_bin (full_path "dot")) graph;
  (* Generate a png from the dot file with an external call to graphviz *)
  ignore (Sys.command ("dot -T " ^ format ^ " -o " ^ full_path format ^ " " ^ full_path "dot"))

let graph_to_string ?(format="svg") graph output_graph = 
  Random.self_init ();
  let rd = Random.int 1000000 in
  let dir_name = "tmp" ^ (string_of_int rd) in
  let full_path ext =
    Fpath.(to_string ((Fpath.v dir_name) // (v "system_output" |> add_ext ext)))
  in
  (* Create output directory if not existing *)
  ignore (Sys.command ("mkdir -p " ^ dir_name));
  (* Write a graphviz dot file *)
  output_graph (Stdlib.open_out_bin (full_path "dot")) graph;
  (* Generate a svg from the dot file with an external call to graphviz *)
  ignore (Sys.command ("dot -T " ^ format ^ " -o " ^ full_path format ^ " " ^ full_path "dot"));

  let graph_string = 
    (full_path format)
    |> Stdlib.open_in_bin
    |> BatPervasives.input_all
  in
  ignore (Sys.command ("rm -r " ^ dir_name));
  graph_string

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program.
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
let print_system ~label ~outdir ~file program =
  (* Definition of some graphviz options how it should be layout *)
  let module Dot = Graph.Graphviz.Dot(struct
                                       include TransitionGraph
                                       let edge_attributes (a, e, b) = [`Label (label e); `Labelfontcolor 16711680; `Color 1234]
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
let print_rvg cache kind ~label ~outdir ~file program =
  let graph = RVG.rvg cache kind program in
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
                       let edge_attributes _ = [`Label ""; `Color 4711; `Labelfontcolor 16711680]
                       let default_edge_attributes _ = []
                       let get_subgraph _ = None
                       let vertex_attributes v = [`Shape `Box; `Color (rv_color v)]
                       let vertex_name v = "\"" ^ label v ^ "\""
                       let default_vertex_attributes _ = []
                       let graph_attributes _ = []
                     end) in
  print_graph outdir (file ^ "_rvg_" ^ show_kind kind) graph Dot.output_graph

(*
    Compute an edge label from a TransitionLabel
    Whenever possible we use unicode representations of mathematic symbols.
  *)
let label l =
    let get_subscript_str i =
      Int.to_string i
      |> String.to_list
      |> List.map (fun c -> "&#832" ^ String.of_char c ^ ";")
      |> String.concat ""
    in
    let gt_id =
      TransitionLabel.gt_id l
      |> get_subscript_str
      |> fun str -> "g" ^ str
    in
    let t_id =
      TransitionLabel.id l
      |> get_subscript_str
      |> fun str -> "t" ^ str
    in
    let prob =
      let p = TransitionLabel.probability l in
      if OurFloat.(equal p one) then "" else
        "p = " ^ OurFloat.to_string p
    in
    let updates =
      let print_update (v,ue) =
        let is_identity = match ue with
          | TransitionLabel.UpdateElement.Poly p ->
              Polynomials.Polynomial.(equal p (of_var v))
          | TransitionLabel.UpdateElement.Dist d -> false
        in
        if is_identity then "" else
          "&eta; (" ^ Var.to_string v ^ ") = " ^ TransitionLabel.UpdateElement.to_short_string ue
      in
      TransitionLabel.update_map l
      |> VarMap.bindings
      |> List.map print_update
      |> List.filter (not % String.is_empty)
      |> String.concat "\n"
    in
    let guard =
      let g = TransitionLabel.guard_without_invariants l in
      if TransitionLabel.Guard.is_true g then "" else
      "&tau; = " ^ TransitionLabel.Guard.to_string g
    in

    let cost =
      let tcost =
        if Polynomials.Polynomial.(equal (TransitionLabel.cost l) one) then ""
        else
          Polynomials.Polynomial.to_string @@ TransitionLabel.cost l
      in
      let gtcost =
        if BoundsInst.RealBound.(equal (TransitionLabel.gtcost l) one) then ""
        else
          BoundsInst.RealBound.show ~complexity:false @@ TransitionLabel.gtcost l
      in
      if String.is_empty (tcost ^ gtcost) then ""
      else
        "\\{" ^ tcost ^ "; " ^ gtcost ^ "\\}"
    in


    [ t_id ^ " &#8712; " ^ gt_id;
      prob;
      updates;
      guard;
      cost
    ]
    |> List.filter (not % String.is_empty)
    |> String.concat "\n"
  
  (* Dot configuration *)
  module DotPaper = Graph.Graphviz.Dot(struct
                                       include TransitionGraph
                                       let edge_attributes (a, e, b) = [`Label (label e)]
                                       let default_edge_attributes _ = []
                                       let get_subgraph _ = None
                                       let vertex_attributes _ = [`Shape `Circle]
                                       let vertex_name v = Location.to_string v
                                       let default_vertex_attributes _ = []
                                       let graph_attributes _ = []
                                     end) 

let print_system_for_paper ?(format="pdf") ~outdir ~file program =
  print_graph ~format:format outdir (file ^ "_graph") (Program.graph program) DotPaper.output_graph

let get_system_for_paper ?(format="svg") program=
  graph_to_string ~format:format (Program.graph program) DotPaper.output_graph


(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the result variable graph of the program.
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
let print_ervg elsb_cache ~label ~outdir ~file program =
  let graph = ERVG.rvg elsb_cache program in
  let module C = Graph.Components.Make(ERVG) in
  let (_,scc_number) = C.scc graph in
  let rv_color (rv: ERV.t) =
    scc_number rv * 424242
  in
  (* Definition of some graphviz options how it should be layout *)
  let module Dot = Graph.Graphviz.Dot(struct
                       include ERVG
                       let edge_attributes _ = [`Label ""; `Color 4711; `Labelfontcolor 16711680]
                       let default_edge_attributes _ = []
                       let get_subgraph _ = None
                       let vertex_attributes v = [`Shape `Box; `Color (rv_color v)]
                       let vertex_name v = "\"" ^ label v ^ "\""
                       let default_vertex_attributes _ = []
                       let graph_attributes _ = []
                     end) in
  print_graph outdir (file ^ "_ervg") graph Dot.output_graph