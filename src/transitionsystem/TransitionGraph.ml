open Batteries

module StdLocation =
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
                       
    let to_string l = "loc_" ^ l.name
                            
    let of_string name = { name }
                       
  end

(*Transitions of Integer Transition Systems*)
module MakeTransition(C : ConstraintTypes.Constraint) =
  struct
    module Constraint_ = C
    module Map = Map.Make(C.Atom_.Polynomial_.Var)

    exception RecursionNotSupported

    type t = {
        name : string;
        start : string;
        target : string;
        update : Constraint_.Atom_.Polynomial_.t Map.t;
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

    let default = {   
        name = "default";
        start = "";
        target = "";
        update = Map.empty;
        guard = C.mk_true;
      }

    let to_string start target transition =
      "TODO"
                
  end

module MakeVariableGraph(T : TransitionGraphTypes.Transition) =
  struct
    module ResultVariable =
      struct
        module Transition_ = T
        type t = unit
        let equal v1 v2 = raise (Failure "Not yet implemented")
        let compare v1 v2 = raise (Failure "Not yet implemented")
        let hash v = raise (Failure "Not yet implemented")
      end

    type t = {
        graph: Graph.Persistent.Digraph.ConcreteBidirectional(ResultVariable).t;
      }

    module Graph = struct
      include Graph.Persistent.Digraph.ConcreteBidirectional(ResultVariable)

      let add_vertices graph vertices =
           vertices
        |> List.map (fun vertex -> fun gr -> add_vertex gr vertex)
        |> List.fold_left (fun gr adder -> adder gr) graph
        
      let add_edges graph edges =
           edges
        |> List.map (fun edge -> fun gr -> add_edge_e gr edge)
        |> List.fold_left (fun gr adder -> adder gr) graph

      let mk vertices edges =
        add_edges (add_vertices empty vertices) edges
    end
  end
  
module MakeTransitionGraph(T : TransitionGraphTypes.Transition) =
  struct
    module Transition_ = T
    module Location_ = StdLocation
    module VariableGraph_ = MakeVariableGraph(T)

    type t = {
        graph: Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location_)(Transition_).t;
        vars: Transition_.Constraint_.Atom_.Polynomial_.Var.t list;
        start: Location_.t;
      }
                     
    module Graph = struct
      include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location_)(Transition_)

      let add_vertices graph vertices =
           vertices
        |> List.map (fun vertex -> fun gr -> add_vertex gr vertex)
        |> List.fold_left (fun gr adder -> adder gr) graph
        
      let add_edges graph edges =
           edges
        |> List.map (fun edge -> fun gr -> add_edge_e gr edge)
        |> List.fold_left (fun gr adder -> adder gr) graph

      let mk vertices edges =
        add_edges (add_vertices empty vertices) edges
    end

    let from vars transitions start =
      let edges = List.map (fun t -> (Location_.of_string (Transition_.start t),
                                      t,
                                      Location_.of_string (Transition_.target t)))
                           transitions in
      let vertices = List.unique (List.append
                                    (List.map (fun (start, _, _) -> start) edges)
                                    (List.map (fun (_, _, target) -> target) edges)) in
      {
        graph = Graph.mk vertices edges;
        vars = vars;
        start = start;
      }

    let create_variable_graph graph =
      raise (Failure "Not yet implemented")

    let graph g = g.graph
      
    let to_string graph =
      "TODO"
      
  end
