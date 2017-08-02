open Batteries

(** Locations of integer transition systems, should end up in a system using ocamlgraph *)
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
                       
    let to_string l = String.concat "_" ["loc"; l.name]
                            
    let of_string name = { name }
                       
  end

(*Transitions of Integer Transition Systems*)
module MakeTransition(C : ConstraintTypes.Constraint) =
  struct
    module Constraint_ = C
    module Map = Map.Make(C.Atom_.Polynomial_.Var)

    type t = {
        name : string;           
        update : Constraint_.Atom_.Polynomial_.t Map.t;
        guard : Constraint_.t;
        (* TODO Transitions should have costs *)
      }

    (* TODO Wrong *)
    let mk name patterns assignments guard vars =
         List.combine vars assignments
      |> List.map (fun (var, assignment) -> Map.add var assignment)
      |> List.fold_left (fun map adder -> adder map) Map.empty 
      |> fun update -> { name; update; guard }
           
    let equal t1 t2 =
      t1.name == t2.name
        
    let compare t1 t2 = 
      if (t1 == t2) then 0
      else if (t1.name < t1.name) then (-1)
      else 1
    
    let default = {   
        name = "default";
        update = Map.empty;
        guard = C.mk_true;
      }

    let to_string start target transition =
      "TODO"
                
  end
  
module MakeTransitionGraph(T : TransitionGraphTypes.Transition) =
  struct
    module Transition_ = T
    module Location_ = StdLocation

    include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location_)(Transition_)

    let add_vertices graph vertices =
         vertices
      |> List.map (fun vertex -> fun gr -> add_vertex gr vertex)
      |> List.fold_left (fun gr adder -> adder gr) graph
      
    let add_edges graph edges =
         edges
      |> List.map (fun edge -> fun gr -> add_edge_e gr edge)
      |> List.fold_left (fun gr adder -> adder gr) graph
      
    let from vars transitions =
      let vertices = List.unique (List.append
                       (List.map (fun (start, _, _) -> Location_.of_string start) transitions)
                       (List.map (fun (_, target, _) -> Location_.of_string target) transitions)) in
      let edges = List.map
                    (fun (start, target, transition) -> (Location_.of_string start, transition, Location_.of_string target))
                    transitions in 
      add_edges (add_vertices empty vertices) edges

    let to_string graph =
      "TODO"
      
  end
