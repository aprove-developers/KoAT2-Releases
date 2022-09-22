(** This module represents a weighted transition graph. *)
module TransitionGraphWeight (Value : PolyTypes.Ring) :
  sig
    (** This module represents a weighted transition graph consisting of a set of locations and a set of transitions and a weight function. We use this module to find shortest paths. Therefore all weights are just 1.*)

    (** Type of weights. *)
    type t = Value.t

    (** Type of transitions. *)
    type edge = TransitionGraph.E.t

    (** Weight function mapping from transitions to one. *)
    val weight : edge -> Value.t

    (** Compares two weights. Always zero as every weight is one. *)
    val compare : 'a -> 'b -> int

    (** Adds two weights. *)
    val add : Value.t -> Value.t -> Value.t

    (** Returns the zero value of our weight type. *)
    val zero : Value.t
  end
