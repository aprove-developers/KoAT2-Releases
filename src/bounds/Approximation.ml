open Batteries

module Transition = Program.Transition
           
type kind = Lower | Upper

type t = {
    time: ((kind * Transition.t), Bound.t) Hashtbl.t;
    size: ((kind * Transition.t * Var.t), Bound.t) Hashtbl.t;
  }


let empty transitioncount varcount = {
    time = Hashtbl.create (2 * transitioncount);
    size = Hashtbl.create (2 * transitioncount * varcount);
  }

                                   
(* Returns the default bound for a kind. *)
let default_bound = function
  | Lower -> Bound.minus_infinity
  | Upper -> Bound.infinity

(* Returns the operator to combine two bounds with the best result. *)
let combine_bounds = function
  | Lower -> Bound.max
  | Upper -> Bound.min

           
let timebound kind appr transition =
  Hashtbl.find_option appr.time (kind, transition)
  |? default_bound kind

let timebound_graph kind appr graph =
  match kind with
  | Lower -> Bound.one
  | Upper -> Program.TransitionGraph.fold_edges_e (fun transition -> Bound.add (timebound Upper appr transition)) (Program.graph graph) Bound.zero

let add_timebound kind bound transition appr =
  Hashtbl.modify (kind, transition) (combine_bounds kind bound) appr.time;
  appr
  

(** A classification is a local size upper bound of a certain form.
        The different classifications are not disjunctive.
        The upcoming classification set always includes the previous one. *)
module Classification = struct

  type t =
    | Equality (** Always smaller or equal to a constant or the value of a prevariable. Examples: x'=x , x'=y , x'=2 *)
    | AddsConstant (** Always smaller or equal to the value of a prevariable plus a constant. Examples: x'=x+1 , x'=y+2 *)
    | ScaledSum (** Always smaller or equal to a scaling factor multiplied with the sum of all prevariables and a constant. Examples: x'=x+y , x'=2*(x+y+z) *)
    | Unbound (** Always smaller or equal to infinity *)

  let template = function
    | Equality -> raise (Failure "TODO: max a (max [x1;...;xn])")
    | AddsConstant -> raise (Failure "TODO: a + max [x1;...;xn]")
    | ScaledSum -> raise (Failure "TODO: a * (b + sum [x1;...;xn])")
    | Unbound -> Bound.infinity
               
  let classify (bound: Bound.t): t =
    raise (Failure "Not yet implemented")

end

let sizebound kind appr transition var =
  Hashtbl.find_option appr.size (kind, transition, var)
  |? default_bound kind      

let add_sizebound kind bound transition var appr =
  Hashtbl.modify (kind, transition, var) (combine_bounds kind bound) appr.size;
  appr      

    (*  end*)
  
