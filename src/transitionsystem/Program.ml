open Batteries
open Constraints
   
module Types =
  struct
   
    module Location =
      struct
        type t = string [@@deriving eq, ord]
               
        let to_string l = l
                        
        let hash l = Hashtbl.hash l
                   
        let of_string name = name               
      end
    module LocationSet = Set.Make(Location)

    module Transition =
      struct
        type t = Location.t * TransitionLabel.t * Location.t

        let equal equal_lbl (l1,t1,l1') (l2,t2,l2') =
          Location.equal l1 l2
          && equal_lbl t1 t2
          && Location.equal l1' l2'
               
        let same =
          equal TransitionLabel.same
          
        let equivalent =
          equal TransitionLabel.equivalent

        let compare compare_lbl (l1,t1,l1') (l2,t2,l2') =
          if Location.compare l1 l2 != 0 then
            Location.compare l1 l2
          else if compare_lbl t1 t2 != 0 then
            compare_lbl t1 t2
          else if Location.compare l1' l2' != 0 then
            Location.compare l1' l2'
          else
            0
          
        let compare_same =
          compare TransitionLabel.compare_same
          
        let compare_equivalent =
          compare TransitionLabel.compare_equivalent

        let add_invariant invariant (l,t,l') =
          (l, TransitionLabel.map_guard (Constraint.mk_and invariant) t, l')
          
        let src (src, _, _) = src
                            
        let label (_, label, _) = label

        let target (_, _, target) = target

        let id =
          TransitionLabel.id % label

        let cost t = TransitionLabel.cost (label t)

        (* TODO Needs to be fast for usage in the timebound hashtables.
       There might be transitions with the same src and target, 
       but that is not a problem for the hashtables,
       since it should not occur very often. *)
        let hash (l,_,l') = Hashtbl.hash (Location.to_string l ^ Location.to_string l')

        let to_id_string (l,label,l') =
          (Int.to_string % TransitionLabel.id) label ^ ": " ^ Location.to_string l ^ "->" ^ Location.to_string l'

        let to_string (l,t,l') =
          to_id_string (l,t,l') ^ ", " ^ TransitionLabel.to_string t
      end
    module TransitionSet = Set.Make(struct include Transition let compare = Transition.compare_same end)

    module TransitionGraph =
      struct
        include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(struct include TransitionLabel let compare = compare_same end)

        let locations graph =
          fold_vertex LocationSet.add graph LocationSet.empty

        let transitions graph =
          fold_edges_e TransitionSet.add graph TransitionSet.empty

        let loc_transitions graph locations =
          transitions graph
          |> TransitionSet.filter (fun (l,_,l') ->
                 List.mem_cmp Location.compare l locations
                 && List.mem_cmp Location.compare l' locations)
          
        module Equivalence_TransitionSet = Set.Make(struct include Transition let compare = Transition.compare_equivalent end)
          
        let equivalent graph1 graph2 =
          LocationSet.equal (locations graph1) (locations graph2)
          && Equivalence_TransitionSet.equal (graph1 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)
                                             (graph2 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)

        let replace_edge_e old_transition new_transition graph =
          add_edge_e (remove_edge_e graph old_transition) new_transition
          
        let add_invariant location invariant graph =
          location
          |> succ_e graph (* An invariant holds before the execution of the successor transitions *)
          |> List.fold_left (fun result transition ->
                 replace_edge_e transition (Transition.add_invariant invariant transition) result
               ) graph          
          
      end

    module RV =
      struct
        type t = Transition.t * Var.t

        let same (t1,v1) (t2,v2) =
          Transition.same t1 t2
          && Var.equal v1 v2

        let equivalent (t1,v1) (t2,v2) =
          Transition.equivalent t1 t2
          && Var.equal v1 v2

        let compare compare_transition (t1,v1) (t2,v2) =
          if compare_transition t1 t2 != 0 then
            compare_transition t1 t2
          else if Var.compare v1 v2 != 0 then
            Var.compare v1 v2
          else
            0

        let compare_same =
          compare Transition.compare_same
          
        let compare_equivalent =
          compare Transition.compare_equivalent

        let hash (t,v) =
          Hashtbl.hash (Transition.to_string t ^ Var.to_string v)
          
        let transition (t,_) = t
                             
        let variable (_,v) = v
                           
        let to_id_string (t,v) =
          "|" ^ Transition.to_id_string t ^ "," ^ Var.to_string v ^ "|"

        let to_string ((l,t,l'), v) =
          Bound.to_string (LocalSizeBound.(sizebound_local `Upper t v |> Option.map as_bound |? default `Upper)) ^ " >= " ^
            to_id_string ((l,t,l'), v) ^ " >= " ^
              Bound.to_string (LocalSizeBound.(sizebound_local `Lower t v |> Option.map as_bound |? default `Lower))
      end

    module RVG =
      struct
        include Graph.Persistent.Digraph.ConcreteBidirectional(struct
                                                                include RV
                                                                let equal = same
                                                                let compare = compare_same
                                                              end)

        type scc = RV.t list

        let rvs_to_id_string rvs =
          rvs
          |> List.map RV.to_id_string
          |> String.concat ","

        let rvs_to_string rvs =
          rvs
          |> List.map RV.to_string
          |> String.concat ","

        let pre rvg rv =
          pred rvg rv
          |> List.enum

        (* TODO Optimizable *)
        let entry_points rvg scc =
          scc
          |> List.enum
          |> Enum.map (pre rvg)
          |> Enum.flatten
          |> Enum.uniq_by RV.same
          |> Util.intersection RV.same (List.enum scc)

        let transitions scc =
          scc
          |> List.enum
          |> Enum.map RV.transition
          |> Enum.uniq_by Transition.same
      end  

  end

open Types
  
type t = {
    graph: TransitionGraph.t;
    vars: Var.t list;
    start: Location.t;
  }

let equal equal_graph program1 program2 =
  equal_graph program1.graph program2.graph
  && List.eq Var.equal program1.vars program2.vars
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

let label_to_transition label =
  (TransitionLabel.start label, label, TransitionLabel.target label)
  
let from transitions start =
  let vars =
    transitions
    |> List.map TransitionLabel.vars
    |> List.fold_left VarSet.union VarSet.empty
    |> VarSet.to_list
  in
  transitions
  |> List.map label_to_transition
  |> List.map (fun (l,t,l') -> (Location.of_string l, t, Location.of_string l'))
  |> fun transitions ->
     if transitions |> List.map Transition.target |> List.mem_cmp Location.compare start then
       raise (Failure "Transition leading back to the initial location.")
     else
       {
         graph = mk (List.enum transitions);
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
            
let transitions =
  TransitionGraph.transitions % graph
  
let pre program (l,t,l') =
  List.enum (TransitionGraph.pred_e (graph program) l)

let add_invariant location invariant =
  map_graph (TransitionGraph.add_invariant location invariant)

let rvg program =
  let add_transition (post_transition: Transition.t) (rvg: RVG.t): RVG.t =
    let rvg_with_vertices: RVG.t = add_vertices_to_rvg (List.map (fun var -> (post_transition,var)) program.vars) rvg in
    let pre_nodes (post_transition: Transition.t) (post_var: Var.t) =
      (* TODO We can maybe try to split upper and lower pre variables. *)
      let vars =
        VarSet.union
          (LocalSizeBound.sizebound_local `Upper (Transition.label post_transition) post_var
           |> Option.map LocalSizeBound.vars
           |? (VarSet.of_list program.vars))
          (LocalSizeBound.sizebound_local `Upper (Transition.label post_transition) post_var
           |> Option.map LocalSizeBound.vars
           |? (VarSet.of_list program.vars))
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

let print_rvg ~label ~outdir ~file program =
  (* Definition of some graphviz options how it should be layout *)
  let module Dot = Graph.Graphviz.Dot(struct
                                       include RVG
                                       let edge_attributes _ = [`Label ""; `Color 4711]
                                       let default_edge_attributes _ = []
                                       let get_subgraph _ = None
                                       let vertex_attributes _ = [`Shape `Box]
                                       let vertex_name v = "\"" ^ label v ^ "\""
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
  
let to_simple_string program =
  TransitionGraph.fold_edges_e (fun t str -> str ^ ", " ^ Transition.to_string t) program.graph "" 
