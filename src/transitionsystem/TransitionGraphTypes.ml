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
    exception RecursionNotSupported
    val mk : name:string ->
             start:string ->
             targets:(string * (Constraint_.Atom_.Polynomial_.t list)) list ->
             patterns:Constraint_.Atom_.Polynomial_.Var.t list ->
             guard:Constraint_.t ->
             vars:Constraint_.Atom_.Polynomial_.Var.t list ->
             t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val start : t -> string
    val target : t -> string
    val default : t
    val to_string : string -> string -> t -> string
  end

(** A graph is a integer transition system based on transitions and locations *)
module type TransitionGraph =
  sig
    module Transition_ : Transition
    module Location_ : Location

    type t

    module Graph : sig
      include module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location_)(Transition_)
      val add_vertices : t -> vertex list -> t
      val add_edges : t -> edge list -> t
      val mk : vertex list -> edge list -> t
    end

    val from : Transition_.Constraint_.Atom_.Polynomial_.Var.t list
               -> Transition_.t list
               -> Location_.t
               -> t

    val to_string : t -> string

  end

