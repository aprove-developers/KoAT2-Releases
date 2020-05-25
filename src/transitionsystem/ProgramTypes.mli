(** Provides default modules to create locations, transitions and transitionsystems. *)
open Batteries
open Polynomials
open Constraints
(** Provides default modules to create locations, transitions and transitionsystems. *)

(** A location is a node of a transition system and can be connected to other locations via transitions. *)
module Location :
sig
  (** Type of location, we use strings. *)
  type t

  (** TODO doc *)
  val equal : t -> t -> bool

  (** TODO doc *)
  val compare : t -> t -> int

  (** Generates a hash value for a location.*)
  val hash : t -> int

  (** Returns a string representing a location. *)
  val to_string : t -> string

  (** Creates a location from a string. *)
  val of_string : string -> t
end

(** A set of locations. *)
module LocationSet : module type of Set.Make(Location)

(** A transition connects two locations and is labeled with an updated function and a guard. *)
module Transition :
sig
  (** Type of a transition, i.e., two connected locations and a label. *)
  type t = Location.t * TransitionLabel.t * Location.t
  
  (** TODO doc *)
  val same : t -> t -> bool

  (** TODO doc *)
  val equivalent : t -> t -> bool

  (** TODO doc *)
  val compare_same : t -> t -> int

  (** TODO doc *)
  val compare_equivalent : t -> t -> int
  
  (** Generates a hash value for a transition. *)
  val hash : t -> int

  (** Returns a string of the form id: src -> target. *)
  val to_id_string : t -> string

  (** Returns a string representing the transition. Parameter {i to_file} is used to get a representation with less special characters. *)
  val to_string : ?to_file:bool -> t -> string
  
  (** Returns the source location of a transiton. *)
  val src : t -> Location.t

  (** Returns the label of a transition. *)
  val label : t -> TransitionLabel.t

  (** Returns the target location of a transition. *)
  val target : t -> Location.t
  
  (** Returns an (unique) id of a transition label. TODO doc unique??*)
  val id : t -> int

  (** Returns a cost function of a transition represented as a polynomial. *)
  val cost : t -> Polynomial.t

  (** Adds the invariant to this transition. *)
  val add_invariant : Constraint.t -> t -> t

  (** TODO doc *)
  val rename : Var.t list -> t -> t

  val rename2 : RenameMap.t -> t -> t
end

(** A set of transitions. *)
module TransitionSet :
sig
  (** A set of transitions. *)

  include module type of Set.Make(struct include Transition let compare = Transition.compare_same end)

  (** TODO doc *)
  val powerset : t -> t Enum.t

  (** Returns a string representing the transition set. *)
  val to_string : t -> string

  val create : ('a -> elt) -> 'a Batteries.Enum.t -> t

end

(** This module represents a transition graph. *)
module TransitionGraph :
sig
  (** This module represents a transition graph consisting of a set of locations and a set of transitions. *)

  include module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(struct include TransitionLabel let compare = compare_same end)

  (** Returns the set of locations. *)
  val locations : t -> LocationSet.t

  (** Returns the set of transitions. *)
  val transitions : t -> TransitionSet.t

  (** Returns the set of transitions consisting of transitions which are in the program graph and where both source and target location are part of the given location list. *)
  val loc_transitions : t -> Location.t list -> TransitionSet.t

  (** TODO doc *)
  val equivalent : t -> t -> bool

  (** Replaces the first edge by the second edge. *)
  val replace_edge_e : Transition.t -> Transition.t -> t -> t

  (** Adds the invariant to the location of the graph. *)
  val add_invariant : Location.t -> Constraint.t -> t -> t
end

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

module IDSet :
sig
  include module type of Set.Make(Batteries.Int)

  val to_string : t -> string
end