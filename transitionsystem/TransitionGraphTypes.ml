open Batteries

(** Provides all module types related to the transition system (program graph) *)

(** A location is a node of a transition system and can be connected to other locations via transitions *)
module type Location =
  sig
    type t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val of_string : string -> t
  end

(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update
    A guard has to be fulfiled for a state to reach another state via the transition
    An update assigns variables a new value as a linear combination of the old values *)
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

(** A graph is a integer transition system based on transitions and locations *)
module type TransitionGraph =
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

