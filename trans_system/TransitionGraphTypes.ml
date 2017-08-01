open Batteries

module type Location =
  sig
    type t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val of_string : string -> t
  end

module type Transition =
  sig
    module Constraint_ : ConstraintTypes.Constraint
    type t
    val mk : string ->
             Constraint_.Atom_.Polynomial_.Var.t list ->
             Constraint_.Atom_.Polynomial_.t list ->
             Constraint_.t ->
             Constraint_.Atom_.Polynomial_.Var.t list ->
             t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val default : t
    val to_string : string -> string -> t -> string
  end

module type Graph =
  sig
    module Transition_ : Transition
    module Location_ : Location
    include module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location_)(Transition_)
    val add_vertices : t -> vertex list -> t
    val add_edges : t -> edge list -> t
    val from : Transition_.Constraint_.Atom_.Polynomial_.Var.t list
               -> (string * string * Transition_.t) list
               -> t
    val to_string : t -> string
  end

