open Batteries

module Make(C : ConstraintTypes.Constraint) =
  struct
    
    module Constraint_ = C

    module TransitionLabel =
      struct
        module Polynomial = Constraint_.Polynomial_
        module Var = Constraint_.Polynomial_.Var
        module Map = Map.Make(Constraint_.Polynomial_.Var)
                   
        exception RecursionNotSupported
                
        module Bound = MinMaxPolynomial.Make(Polynomial)
       
        type kind = Lower | Upper

        type t = {
            name : string;
            start : string;
            target : string;
            update : Polynomial.t Map.t;
            guard : Constraint_.t;
            (* TODO Transitions should have costs *)
          }
               
        (* TODO Pattern <-> Assigment relation *)
        let mk ~name ~start ~targets ~patterns ~guard ~vars =
          if List.length targets != 1 then raise RecursionNotSupported else
            let (target, assignments) = List.hd targets in
            List.combine patterns assignments
            |> List.map (fun (var, assignment) -> Map.add var assignment)
            |> List.fold_left (fun map adder -> adder map) Map.empty 
            |> fun update -> { name; start; target; update; guard }
                           
        let equal t1 t2 =
          t1.name == t2.name
          
        let compare t1 t2 = 
          if (t1 == t2) then 0
          else if (t1.name < t1.name) then (-1)
          else 1
          
        let start t = t.start
                    
        let target t = t.target
                     
        let update t var = Map.Exceptionless.find var t.update                    
                 
        let guard t = t.guard
                    
        let default = {   
            name = "default";
            start = "";
            target = "";
            update = Map.empty;
            guard = C.mk_true;
          }
                    
        let sizebound_local kind label var =
          (* If we have an update pattern, it's like x'=b and therefore x'<=b and x >=b and b is a bound for both kinds. *)
          (* TODO Should we also try to substitute vars in the bound if it leads to a simpler bound? E.g. x<=10 && x'=x : b:=x or b:=10? *)
          match update label var with
          | Some bound -> Bound.of_poly bound
          | None ->
             match kind with
             (* TODO Use SMT-Solving to find bounds *)
             | Upper -> Bound.infinity
             | Lower -> Bound.minus_infinity

        let update_to_string_list update =
          if Map.is_empty update then
            "true"
          else
            let entry_string var poly = Var.to_string var ^ "' := " ^ Polynomial.to_string poly
            and ((var, poly), without_first) = Map.pop update in
            Map.fold (fun var poly result -> result ^ " && " ^ entry_string var poly) without_first (entry_string var poly)

        let to_string label =          
          let guard = if Constraint_.is_true label.guard then "" else " && " ^ Constraint_.to_string label.guard in
          update_to_string_list label.update ^ guard
          
      end

    module Location =
      struct
        type t = {
            name : string;
            (* TODO Possible optimization: invariant : PolynomialConstraints.t*)
          } [@@deriving eq, ord]
               
        (*Needed by ocamlgraph*)    
        let hash l = Hashtbl.hash l.name
                   
        let to_string l = l.name
                        
        let of_string name = { name }
                           
      end

    module TransitionGraph = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(TransitionLabel)

    module Transition = struct
      include TransitionGraph.E
      let to_string (l,t,l') = Location.to_string l ^ "->" ^ Location.to_string l' ^ ", " ^ TransitionLabel.to_string t
      let equal = (=)
    end

    module RV =
      struct
        type t = Transition.t * Constraint_.Polynomial_.Var.t [@@deriving eq, ord]
        let hash v = raise (Failure "Not yet implemented")
        let transition (t,v) = t
        let variable (t,v) = v
        let to_string (t,v) = Transition.to_string t ^ "," ^ Constraint_.Polynomial_.Var.to_string v
      end
    module RVG = Graph.Persistent.Digraph.ConcreteBidirectional(RV)

    type t = {
        graph: TransitionGraph.t;
        vars: Constraint_.Polynomial_.Var.t list;
        start: Location.t;
      }
                     
    let add_vertices graph vertices =
      vertices
      |> List.map (fun vertex -> fun gr -> TransitionGraph.add_vertex gr vertex)
      |> List.fold_left (fun gr adder -> adder gr) graph
      
    let add_edges graph edges =
      edges
      |> List.map (fun edge -> fun gr -> TransitionGraph.add_edge_e gr edge)
      |> List.fold_left (fun gr adder -> adder gr) graph
      
    let mk vertices edges =
      add_edges (add_vertices TransitionGraph.empty vertices) edges

    let from vars transitions start =
      let edges = List.map (fun t -> (Location.of_string (TransitionLabel.start t),
                                      t,
                                      Location.of_string (TransitionLabel.target t)))
                           transitions in
      let vertices = List.unique (List.append
                                    (List.map (fun (start, _, _) -> start) edges)
                                    (List.map (fun (_, _, target) -> target) edges)) in
      {
        graph = mk vertices edges;
        vars = vars;
        start = start;
      }

    let vars program =
      Set.of_list program.vars

    let add_vertices_to_rvg vertices rvg =
      vertices
      |> List.map (flip RVG.add_vertex)
      |> List.fold_left (fun rvg adder -> adder rvg) rvg

    let graph g = g.graph

    let pre program (l,t,l') =
      Set.of_list (TransitionGraph.pred_e (graph program) l)

    let rvg program =
      let add_transition post_transition (rvg: RVG.t) =
        let rvg_with_vertices: RVG.t = add_vertices_to_rvg (List.map (fun var -> (post_transition,var)) program.vars) rvg in
        let pre_nodes post_transition post_var =
          TransitionLabel.sizebound_local TransitionLabel.Upper (Transition.label post_transition) post_var
          |> TransitionLabel.Bound.vars
          |> Set.cartesian_product (pre program post_transition)
          |> Set.map (fun (pre_transition,pre_var) -> (pre_transition,pre_var,post_var))
        in
        Set.map (pre_nodes post_transition) (vars program)
        |> fun set -> Set.fold Set.union set Set.empty
        |> fun set -> Set.fold (fun (pre_transition,pre_var,post_var) rvg -> RVG.add_edge rvg (pre_transition,pre_var) (post_transition,post_var)) set rvg_with_vertices                          
      in
      TransitionGraph.fold_edges_e add_transition program.graph RVG.empty
      
    let print_graph name graph output_graph =
      let out_dir = "output" in
      (* Create output directory if not existing *)
      ignore (Sys.command ("mkdir -p " ^ out_dir));
      (* Write a graphviz dot file *)
      output_graph (Pervasives.open_out_bin (out_dir ^ "/" ^ name ^ ".dot")) graph;
      (* Generate a png from the dot file with an external call to graphviz *)
      ignore (Sys.command ("dot -T png -o " ^ (out_dir ^ "/" ^ name ^ ".png ") ^ (out_dir ^ "/" ^ name ^ ".dot")))
                
    let print_system name program =
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
      print_graph (name ^ "_system") (graph program) Dot.output_graph

    let print_rvg name program =
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
      print_graph (name ^ "_rvg") (rvg program) Dot.output_graph

    let is_initial graph (l,t,l') =
      graph.start == l

    let to_string graph =
      "TODO"
      
  end
