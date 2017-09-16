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
          }
               
        let equal l1 l2 = l1.name == l2.name (*&& (Variables.equal_varlist l1.vars l2.vars)*)
                        
        let compare l1 l2 = 
          if equal l1 l2 then 0
          else if l1.name < l2.name then (-1)
          else 1
          
        (*Needed by ocamlgraph*)    
        let hash l = Hashtbl.hash l.name
                   
        let to_string l = l.name
                        
        let of_string name = { name }
                           
      end

    module TransitionGraph = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(TransitionLabel)

    module Transition = TransitionGraph.E

    module RV =
      struct
        type t = Transition.t * Constraint_.Polynomial_.Var.t
        let equal v1 v2 = raise (Failure "Not yet implemented")
        let compare v1 v2 = raise (Failure "Not yet implemented")
        let hash v = raise (Failure "Not yet implemented")
        let transition (t,v) = t
        let variable (t,v) = v
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

    let rvg graph =
      raise (Failure "Not yet implemented")

    let graph g = g.graph

    let print_graph name program =
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
      let out_dir = "output" in
      (* Create output directory if not existing *)
      ignore (Sys.command ("mkdir " ^ out_dir));
      (* Write a graphviz dot file *)
      Dot.output_graph (Pervasives.open_out_bin (out_dir ^ "/" ^ name ^ ".dot")) (graph program);
      (* Generate a png from the dot file with an external call to graphviz *)
      ignore (Sys.command ("dot -T png -o " ^ (out_dir ^ "/" ^ name ^ ".png ") ^ (out_dir ^ "/" ^ name ^ ".dot")))

                
    let is_initial graph (l,t,l') =
      graph.start == l

    let to_string graph =
      "TODO"
      
  end
