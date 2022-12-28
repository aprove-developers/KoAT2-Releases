(** Module provides methods to print a program or a result variable graph to png file. *)
open ProgramModules
open RVGTypes

module VarMap = ProgramTypes.VarMap

let print_graph ~format out_dir name graph output_graph =
let full_path ext =
    Fpath.(to_string (out_dir // (v name |> add_ext ext)))
in
(* Create output directory if not existing *)
ignore (Sys.command ("mkdir -p " ^ Fpath.to_string out_dir));
(* Write a graphviz dot file *)
output_graph (Stdlib.open_out_bin (full_path "dot")) graph;
(* Generate a png from the dot file with an external call to graphviz *)
ignore (Sys.command ("dot -T"^ format ^ " -o " ^ full_path format ^ " " ^ full_path "dot"))

let print_graph_to_string ~format graph output_graph =
let rec read_from_channel inp_chann =
    try let next_line = input_line inp_chann in
        next_line ^ "\n" ^ read_from_channel inp_chann
    with End_of_file -> ""
in

try
    let (graphviz_in, graphviz_out) = Unix.open_process ("dot -T"^format ^ " 2>/dev/null") in
    output_graph graphviz_out graph;
    close_out graphviz_out;
    Some (read_from_channel graphviz_in)
with
    (* Some Programs lead to huge graphs and huge edge labels that dot cannot handle. *)
    Sys_error e -> None


module Make(PM: ProgramTypes.ProgramModules) = struct
  open PM
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
                                         let vertex_name v = "\""^Location.to_string v^"\""
                                         let default_vertex_attributes _ = []
                                         let graph_attributes _ = []
                                       end) in
    print_graph outdir (file ^ "_system") (Program.graph program) Dot.output_graph

  open Batteries

  (* Compute an edge label from a TransitionLabel
     Whenever possible we use unicode representations of mathematic symbols.  *)
  let label l =
      let t_ids_str =
        TransitionLabel.ids_to_string ~pretty:true l
      in
      let updates =
        let print_update (v,p) =
          let is_identity = UpdateElement.(equal p (of_var v)) in
          if is_identity then "" else
            "&eta; (" ^ Var.to_string  ~pretty:true v ^ ") = " ^ UpdateElement.to_string_pretty p
        in
        TransitionLabel.update_map l
        |> VarMap.bindings
        |> List.map print_update
        |> List.filter (not % String.is_empty)
        |> String.concat "\n"
      in
      let guard =
        let g = TransitionLabel.guard l in
        if Guard.is_true g then "" else
        "&tau; = " ^ Guard.to_string ~pretty:true g
      in

      let cost =
        let cost_poly =
          if Polynomials.Polynomial.(equal (TransitionLabel.cost l) one) then ""
          else Polynomials.Polynomial.to_string @@ TransitionLabel.cost l
        in
        if String.is_empty cost_poly then ""
        else
          "\\{" ^ cost_poly ^ "\\}"
      in


      [ t_ids_str
      ; updates
      ; guard
      ; cost
      ]
      |> List.filter (not % String.is_empty)
      |> String.concat "\n"

  type color = Black | Red | Blue | Green | Yellow | Purple | Brown | White [@@deriving ord, eq]

  let get_color = function
    | Black -> `Color 0
    | Red -> `Color 16711680
    | Blue -> `Color 255
    | Green -> `Color 32768
    | Yellow -> `Color 16776960
    | Purple -> `Color 800080
    | Brown -> `Color 10824234
    | White -> `Color 16777215

  module TransitionMap = Map.Make(Transition)


  let print_system_pretty ?(format="pdf") ?(color_map = TransitionMap.empty) program =
    let module DotPretty = Graph.Graphviz.Dot(struct
                                               include TransitionGraph
                                               let edge_attributes (a, e, b) = [`Label (label e);
                                                 if not (TransitionMap.mem (a, e, b) color_map) then
                                                   get_color Black else
                                                   get_color (TransitionMap.find (a, e, b) color_map)]
                                               let default_edge_attributes _ = []
                                               let get_subgraph _ = None
                                               let vertex_attributes _ = [`Shape `Circle]
                                               let vertex_name v = "\""^Location.to_string v^"\""
                                               let default_vertex_attributes _ = []
                                               let graph_attributes _ = []
                                     end)
    in
    print_graph_to_string ~format:format (Program.graph program) DotPretty.output_graph

 let print_system_pretty_html color_map program = match print_system_pretty ~format:"svg" ~color_map program with
   | None -> ""
   | Some system ->
       let divid = unique () in
       "<button onclick=\"showgraph" ^ string_of_int divid ^ "()\">Show Graph</button>\n" ^
       "<div id=\"graph" ^ string_of_int divid ^ "\" style=\"display:none\">\n" ^
       system ^
       "</div>\n
       <script>
         function showgraph" ^ string_of_int divid ^ "() {
           var x = document.getElementById(\"graph" ^ string_of_int divid ^ "\");
           if (x.style.display === \"none\") {
             x.style.display = \"block\";
           } else {
             x.style.display = \"none\";
           }
         }
       </script>"
end

(** RVGs are only defined for classical programs, since otherwise we do not know the local size bounds *)
module MakeFromClassical(PM: ProgramTypes.ClassicalProgramModules) = struct
    include Make(PM)

    module RVG = MakeRVG(PM)
    module LSB = LocalSizeBound.Make(PM.TransitionLabel)(PM.Transition)(PM.Program)

    (** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the result variable graph of the program.
            For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
    let print_rvg ~label ~outdir ~file program =
    let graph =
      RVG.rvg
        (fun(t,v) ->
          LSB.compute_bound (PM.Program.input_vars program) t v
          |> Option.map Batteries.(LSB.vars % Batteries.Tuple2.first))
        program
    in
    let module C = Graph.Components.Make(RVG) in
    let (_,scc_number) = C.scc graph in
    let rv_color (rv: PM.RV.t) =
        scc_number rv * 424242
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
    print_graph outdir (file ^ "_rvg") graph Dot.output_graph
end

include MakeFromClassical(ProgramModules)
