open Batteries
open Constraints
open Polynomials
(** Provides commonly used module types in programs *)

(** A location is a node of a transition system and can be connected to other locations via transitions. *)
module type Location = sig
  (** Type of location, we use strings. *)
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  (** Generates a hash value for a location.*)
  val hash : t -> int

  (** Returns a string representing a location. *)
  val to_string : t -> string
end

(** A transition connects two locations and is labeled with an updated function and a guard. *)
module type Transition = sig
  type location
  (** Type of a transition, i.e., two connected locations and a label. *)
  type t = location * TransitionLabel.t * location

  val equal : t -> t -> bool

  val same : t -> t -> bool

  val equivalent : t -> t -> bool

  val compare_same : t -> t -> int

  val compare_equivalent : t -> t -> int

  (** Generates a hash value for a transition. *)
  val hash : t -> int

  val overapprox_nonlinear_updates: t -> t

  (** Returns a string of the form id: src -> target. *)
  val to_id_string : t -> string
  val to_id_string_pretty : t -> string

  (** Returns a string representing the transition. Parameter {i to_file} is used to get a representation with less special characters. *)
  val to_string : ?to_file:bool -> t -> string

  val to_string_pretty : t -> string

  (** Returns the source location of a transiton. *)
  val src : t -> location

  (** Returns the label of a transition. *)
  val label : t -> TransitionLabel.t

  (** Apply a function to a transitions label *)
  val map_label: (TransitionLabel.t -> TransitionLabel.t) -> t -> t

  (** Returns the target location of a transition. *)
  val target : t -> location

  (** Returns an (unique) id of a transition label. TODO doc unique??*)
  val id : t -> int

  (** Returns a cost function of a transition represented as a polynomial. *)
  val cost : t -> Polynomial.t

  (** Adds the invariant to this transition. *)
  val add_invariant : Constraint.t -> t -> t

  val rename : RenameMap.t -> t -> t
end

module type TransitionSet = sig
  (** A set of transitions. *)

  include Set.S
  type locationSet

  (** TODO doc *)
  val powerset : t -> t Enum.t

  (** Returns a string representing the transition set. *)
  val to_string : t -> string

  (** Returns a short string representing the transition set. *)
  val to_id_string : t -> string

  val create : ('a -> elt) -> 'a Batteries.Enum.t -> t

  val locations: t -> locationSet
end

(** This module represents a transition graph. *)
module type TransitionGraph = sig
  module Location: Location
  module Transition: Transition
    with type location = Location.t
    and type t = Location.t * TransitionLabel.t * Location.t
  module LocationSet: Set.S with type elt = Location.t
  module TransitionSet : TransitionSet
    with type elt = Transition.t
    and type locationSet = LocationSet.t

  include Graph.Sig.P with type V.t = Location.t
    and type V.label = Location.t
      and type E.t = Location.t * TransitionLabel.t * Location.t
      and type E.label = TransitionLabel.t

  (** Creates a transition graph from an enum of transitions. *)
  val mk : Transition.t Enum.t -> t

  (** Adds all locations from an enum to a transtion graph. *)
  val add_locations : Location.t Enum.t -> t -> t

  (** Adds all transitions from an enum to a transtion graph. Implicitly adds locations when they do not exit. *)
  val add_transitions : Transition.t Enum.t -> t -> t

  (** Returns the set of locations. *)
  val locations : t -> LocationSet. t

  (** Returns the set of transitions. *)
  val transitions : t -> TransitionSet.t

  (** Returns the set of transitions consisting of transitions which are in the program graph and where both source and target location are part of the given location list. *)
  val loc_transitions : t -> Location.t list -> TransitionSet.t

  (** Checks fore equivalence TODO: lies everywhere...*)
  val equivalent : t -> t -> bool

  (** Replaces the first edge by the second edge. *)
  val replace_edge_e : Transition.t -> Transition.t -> t -> t

  (** Adds the invariant to the location of the graph. *)
  val add_invariant : Location.t -> Constraint.t -> t -> t
end

module type Program = sig
  (* module Location : Location *)
  type location
  module Transition : Transition with type location = location
  module LocationSet: Set.S with type elt = location
  module TransitionSet : TransitionSet
    with type elt = Transition.t
    and type locationSet = LocationSet.t
  module TransitionGraph : TransitionGraph
    with type Location.t = location
    and type LocationSet.t = LocationSet.t
    and type Transition.t = Transition.t
    and type TransitionSet.t = TransitionSet.t
    and type TransitionSet.locationSet = LocationSet.t

  (** Type of a program consisting of a program graph and a start location. *)
  type t

  (** Removes the location from the program and all edges to it. *)
  val remove_location : t -> location -> t

  (** Removes a transition from a program. *)
  val remove_transition : t -> Transition.t -> t

  (* Removes the transitions from a certain transitionset to a program *)
  val remove_TransitionSet: TransitionSet.t -> t -> t

  (** TODO doc *)
  val map_graph : (TransitionGraph.t -> TransitionGraph.t) -> t -> t

  (** Creates a program from a list of transitions and a (start) location. A list of k transitions makes up a Com_k transition *)
  val from : Transition.t list list -> location -> t

  (** Returns transition graph of a program. *)
  val graph : t -> TransitionGraph.t

  (** Adds the invariant to a location of the program. *)
  val add_invariant : location -> Constraint.t -> t -> t

  (** Tries to simplify the guard of all transitions by invoking the SMT Solver *)
  val simplify_all_guards : t -> t

  (** Returns a set of all transitions which occur directly before the given transition in the graph.
      Corresponds to pre(t).
      Note that the computation involves calls to the SMT solver and is therefore expensive.
      The returned Enum is lazy. *)
  val pre : t -> Transition.t -> Transition.t Enum.t

  (** A cached version of pre. The identifier for the cache is the transition id (the program is not considered) *)
  val pre_transitionset_cached: t -> Transition.t -> TransitionSet.t
  (** Reset the cache for pre_cached *)
  val reset_pre_cache: unit -> unit

  (** Returns true if the given transition is an initial transition. *)
  val is_initial : t -> Transition.t -> bool

  (** Returns true if the given transition is an initial transition. *)
  val is_initial_location : t -> location -> bool

  (** Returns true if the program graphs are equivalent and both start locations are equal. *)
  val equivalent : t -> t -> bool

  (** Returns a formatted string representing the program. *)
  val to_formatted_string: ?pretty:bool -> t -> FormattedString.t

  (** Returns a string representing the program. *)
  val to_string : t -> string

  (** Returns a string representing the program that can be dumped to a KoAT input file. *)
  val to_file : t -> string

  (** Input is not interpreted as a filepath, but as a program in simple mode. Method returns a string representation of a program from such an input. *)
  val to_simple_string : t -> string

  (** Returns all variables of the program. *)
  val vars : t -> VarSet.t

  (** Returns all input variables of the program. *)
  val input_vars : t -> VarSet.t

  (** Returns the number of variables. *)
  val cardinal_vars : t -> int

  (** Returns all locations which occur in the transitions, but each location only once. *)
  val locations : t -> LocationSet.t

  (** Returns a set of all transitions which occur in the program graph of the program. *)
  val transitions : t -> TransitionSet.t

  (** Returns start location. *)
  val start : t -> location

  (** Returns the (biggest) strongly connected components of the transiton graph. *)
  val sccs : t -> TransitionSet.t Enum.t

  (** Returns the number of transition involved in a scc. *)
  val cardinal_trans_scc : t -> int

  (** Returns all transitions which are parallel to a given transition. Thus, all transitions start in the same location and end in the same location. *)
  val parallelTransitions : t -> Transition.t -> TransitionSet.t

  (** Returns all transitions, that belong to an SCC. *)
  val non_trivial_transitions : t -> TransitionSet.t

  (** Creates a file (if it does not already exist) and writes the program into it. *)
  val to_file : t -> string -> unit

  (** Computes all entry transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  val entry_transitions : Batteries.Logger.log -> t -> Transition.t list -> Transition.t Batteries.List.t

  (** Computes all outgoing transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  val outgoing_transitions : Batteries.Logger.log -> t -> Transition.t list -> Transition.t Batteries.List.t
end
