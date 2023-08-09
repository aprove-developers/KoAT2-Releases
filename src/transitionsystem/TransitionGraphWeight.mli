(** This module represents a weighted transition graph. *)
module TransitionGraphWeight (Value : PolyTypes.Ring) : sig
  (** This module represents a weighted transition graph consisting of a set of locations and a set of transitions and a weight function. We use this module to find shortest paths. Therefore all weights are just 1.*)

  type t = Value.t
  (** Type of weights. *)

  type edge = TransitionGraph_.E.t
  (** Type of transitions. *)

  val weight : edge -> Value.t
  (** Weight function mapping from transitions to one. *)

  val compare : 'a -> 'b -> int
  (** Compares two weights. Always zero as every weight is one. *)

  val add : Value.t -> Value.t -> Value.t
  (** Adds two weights. *)

  val zero : Value.t
  (** Returns the zero value of our weight type. *)
end
