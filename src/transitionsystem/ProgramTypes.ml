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

module type TransitionSet = sig
  (** A set of transitions. *)

  include Set.S
  type location_set

  (** TODO doc *)
  val powerset : t -> t Enum.t

  (** Returns a string representing the transition set. *)
  val to_string : t -> string

  (** Returns a short string representing the transition set. *)
  val to_id_string : t -> string

  val create : ('a -> elt) -> 'a Batteries.Enum.t -> t

  val locations: t -> location_set

  (** Returns a locationSet corresponding to the targets of all transitions contained in the set passed as first argument *)
  val targets: t -> location_set
end


(** A transition connects two locations and is labeled with an updated function and a guard. *)
module type Transition = sig
  type location

  (** Type of a transition, i.e., two connected locations and a label. *)
  type t = location * TransitionLabel.t * location

  val equal : t -> t -> bool

  val same : t -> t -> bool

  val equivalent : t -> t -> bool

  (** default to compare_same, i.e., comparison of ids *)
  val compare: t -> t -> int

  val compare_same : t -> t -> int

  val compare_equivalent : t -> t -> int

  (** Generates a hash value for a transition. *)
  val hash : t -> int

  val overapprox_nonlinear_updates: t -> t

  (** Returns a string of the form id: src -> target. *)
  val to_id_string : t -> string
  val to_id_string_pretty : t -> string

  (** Returns a string representing the transition. *)
  val to_string : t -> string

  val to_string_pretty : t -> string

  (** Returns the source location of a transition. *)
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

(** This module represents a transition graph. *)
module type TransitionGraph = sig
  type location
  type location_set
  type transition = location * TransitionLabel.t * location
  type transition_set


  include Graph.Sig.P with type V.t = location
    and type V.label = location
      and type E.t = transition
      and type E.label = TransitionLabel.t

  (** Creates a transition graph from an enum of transitions. *)
  val mk : transition Enum.t -> t

  (** Adds all locations from an enum to a transtion graph. *)
  val add_locations : location Enum.t -> t -> t

  (** Adds all transitions from an enum to a transtion graph. Implicitly adds locations when they do not exit. *)
  val add_transitions : transition Enum.t -> t -> t

  (** Apply function to the graphs transitions  *)
  val map_transitions: (transition -> transition) -> t -> t

  (** Apply function to the graphs labels  *)
  val map_labels: (TransitionLabel.t -> TransitionLabel.t) -> t -> t

  (** Returns the set of locations. *)
  val locations : t -> location_set

  (** Returns the set of transitions. *)
  val transitions : t -> transition_set

  (** Returns the set of transitions consisting of transitions which are in the program graph and where both source and target location are part of the given location list. *)
  val loc_transitions : t -> location list -> transition_set

  (** Checks fore equivalence TODO: lies everywhere...*)
  val equivalent : t -> t -> bool

  (** Replaces the first edge by the second edge. *)
  val replace_edge_e : transition -> transition -> t -> t

  (** Adds the invariant to the location of the graph. *)
  val add_invariant : location -> Constraint.t -> t -> t
end

module type Program = sig
  type location

  type transition = location * TransitionLabel.t * location

  type location_set
  type transition_set

  type transition_graph

  (** Type of a program consisting of a program graph and a start location. *)
  type t

  (** Create a program from a start location and an enum of transitions *)
  val from_enum: location -> transition Enum.t -> t

  (** Create a program from a start location and a graph *)
  val from_graph: location -> transition_graph -> t

  (** Removes the location from the program and all edges to it. *)
  val remove_location : t -> location -> t

  (** Removes a transition from a program. *)
  val remove_transition : t -> transition -> t

  (** Apply function to the underlying TransitionGraph *)
  val map_graph : (transition_graph -> transition_graph) -> t -> t

  (** Apply function to the programs transitions  *)
  val map_transitions: (transition -> transition) -> t -> t

  (** Apply function to the programs labels  *)
  val map_labels: (TransitionLabel.t -> TransitionLabel.t) -> t -> t

  (** Returns transition graph of a program. *)
  val graph : t -> transition_graph

  (** Adds the invariant to a location of the program. *)
  val add_invariant : location -> Constraint.t -> t -> t

  (** Tries to simplify the guard of all transitions by invoking the SMT Solver *)
  val simplify_all_guards : t -> t

  (** Returns a set of all transitions which occur directly before the given transition in the graph.
      Corresponds to pre(t).
      Note that the computation involves calls to the SMT solver and is therefore expensive.
      The returned Enum is lazy. *)
  val pre : t -> transition -> transition Enum.t

  (** A cached version of pre. The identifier for the cache is the transition id (the program is not considered) *)
  val pre_transitionset_cached: t -> transition -> transition_set
  (** Reset the cache for pre_cached *)
  val reset_pre_cache: unit -> unit

  (** Returns true if the given transition is an initial transition. *)
  val is_initial : t -> transition -> bool

  (** Returns true if the given transition is an initial transition. *)
  val is_initial_location : t -> location -> bool

  (** Returns true if the program graphs are equivalent and both start locations are equal. *)
  val equivalent : t -> t -> bool

  (** Returns a formatted string representing the program. *)
  val to_formatted_string: ?pretty:bool -> t -> FormattedString.t

  (** Returns a string representing the program. *)
  val to_string : t -> string

  (** Input is not interpreted as a filepath, but as a program in simple mode. Method returns a string representation of a program from such an input. *)
  val to_simple_string : t -> string

  (** Returns all variables of the program. *)
  val vars : t -> VarSet.t

  (** Returns all input variables of the program. *)
  val input_vars : t -> VarSet.t

  (** Returns all locations which occur in the transitions, but each location only once. *)
  val locations : t -> location_set

  (** Returns a set of all transitions which occur in the program graph of the program. *)
  val transitions : t -> transition_set

  (** Returns start location. *)
  val start : t -> location

  (** Returns the (biggest) strongly connected components of the transiton graph. *)
  val sccs : t -> transition_set Enum.t

  (** Returns all transitions which are parallel to a given transition. Thus, all transitions start in the same location and end in the same location. *)
  val parallel_transitions : t -> transition -> transition_set

  (** Returns all transitions, that belong to an SCC. *)
  val non_trivial_transitions : t -> transition_set

  (** Computes all entry transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  val entry_transitions : Batteries.Logger.log -> t -> transition list -> transition Batteries.List.t

  (** Computes all outgoing transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  val outgoing_transitions : Batteries.Logger.log -> t -> transition list -> transition Batteries.List.t
end
