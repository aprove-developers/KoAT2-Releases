open Batteries
open Constraints
open Polynomials
(** Provides commonly used module types in programs *)

module VarMap = Map.Make(Var) (* Useful module *)

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

module type TransitionLabel = sig
  (** A transition label consists of an unique id, an update function, a guard and a cost function. *)
  type t
  type update_element
  module Invariant = Guard

  (** Returns a default label with id 0, [true] as the guard,no update function and the default cost function. *)
  val default: t

  (** Returns the update map of the transitionlabel *)
  val update_map : t -> update_element VarMap.t

  (** Returns the update of a variable. *)
  val update : t -> Var.t -> update_element Option.t

  (** TODO doc *)
  val normalise : t -> VarSet.t -> t

  (** Returns if the two labels are the same entity. *)
  val same: t -> t -> bool

  (** Returns if the two labels describe the same transition *)
  val equivalent: t -> t -> bool

  (** Compare IDs *)
  val compare_same: t -> t -> int
  (** TODO doc *)
  val compare_equivalent: t -> t -> int
  (** This should default to compare_same *)
  val compare: t -> t -> int

  (** Returns the guard of the label. *)
  val guard: t -> Guard.t

  (** Returns the guard of the label without considering invariants. *)
  val guard_without_inv : t -> Guard.t

  (** Returns the invariant. *)
  val invariant : t -> Invariant.t

  (** Apply function to guard *)
  val map_guard: (Guard.t -> Guard.t) -> t -> t

  (** Returns the unique id. *)
  val id: t -> int

  val add_invariant: t -> Invariant.t -> t

  (** Returns the cost function *)
  val cost : t -> Polynomials.Polynomial.t

  (** Returns a string representing the label. *)
  val to_string : ?pretty:bool -> t -> string

  (** Returns a string representing the left hand side of the update function. *)
  val update_to_string_lhs : t -> string

  (** Returns a string representing the right hand side of the update function. *)
  val update_to_string_rhs : t -> string

  val update_to_string_lhs_pretty : t -> string

  val update_to_string_rhs_pretty : t -> string

  (** Returns a string representing the cost. *)
  val cost_to_string : t -> string

  (** Returns a string representing the id of the label. *)
  val to_id_string : t -> string

  (** The call {i fill_up_arg_vars_up_to_num n} adds trivial updates for the first {i n}, i.e. Arg_0, .., Arg_n-1 arguments that are not contained in the labels update map *)
  val fill_up_arg_vars_up_to_num: int -> t -> t

  (** TODO doc *)
  val rename : RenameMap.t -> t -> t

  (** Rename temporary variables to identifiers provided by the (possibly infinite) lazy list *)
  val rename_temp_vars : t -> Var.t LazyList.t -> t

  (** Overapproximates nonlinear updates by nondeterministic updates. Useful for Farkas lemma *)
  val overapprox_nonlinear_updates : t -> t

  (** Returns the set of variables. *)
  val vars : t -> VarSet.t

  val vars_without_memoization : t -> VarSet.t

  (** Returns the set of input variables of the transition, i.e. the non temporary variables  *)
  val input_vars : t -> VarSet.t

  (** Returns the number of input variables *)
  val input_size: t -> int

  (** Guard that is true if both transitions can be executed one after another *)
  val chain_guards: t -> t -> Guard.t

  (** Overapproximates nonlinear updates by nondeterministic updates. Useful for Farkas lemma *)
  val overapprox_nonlinear_updates : t -> t

  val remove_non_contributors : VarSet.t -> t -> t
end

(** A transition connects two locations and is labeled with an updated function and a guard. *)
module type Transition = sig
  type location
  type transition_label

  (** Type of a transition, i.e., two connected locations and a label. *)
  type t = location * transition_label * location

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
  val label : t -> transition_label

  (** Apply a function to a transitions label *)
  val map_label: (transition_label -> transition_label) -> t -> t

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
  type transition_label
  type transition = location * transition_label * location
  type transition_set


  include Graph.Sig.P with type V.t = location
    and type V.label = location
      and type E.t = transition
      and type E.label = transition_label

  (** Creates a transition graph from an enum of transitions. *)
  val mk : transition Enum.t -> t

  (** Adds all locations from an enum to a transtion graph. *)
  val add_locations : location Enum.t -> t -> t

  (** Adds all transitions from an enum to a transtion graph. Implicitly adds locations when they do not exit. *)
  val add_transitions : transition Enum.t -> t -> t

  (** Apply function to the graphs transitions  *)
  val map_transitions: (transition -> transition) -> t -> t

  (** Apply function to the graphs labels  *)
  val map_labels: (transition_label -> transition_label) -> t -> t

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
  type transition_label

  type transition = location * transition_label * location

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
  val map_labels: (transition_label -> transition_label) -> t -> t

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

module type ProgramModules = sig
  module Location: Location
  module LocationSet: Set.S with type elt = Location.t

  module UpdateElement: PolyTypes.Polynomial
    with type value = OurInt.t

  module TransitionLabel: TransitionLabel
    with type update_element = UpdateElement.t

  module Transition: Transition
    with type location = Location.t
     and type transition_label = TransitionLabel.t

  module TransitionSet: TransitionSet
    with type elt = Transition.t
     and type location_set = LocationSet.t

  module TransitionGraph: TransitionGraph
    with type location = Location.t
     and type location_set = LocationSet.t
     and type transition_label = TransitionLabel.t
     and type transition = Transition.t
     and type transition_set = TransitionSet.t

  module Program: Program
    with type location = Location.t
     and type location_set = LocationSet.t
     and type transition_label = TransitionLabel.t
     and type transition = Transition.t
     and type transition_set = TransitionGraph.transition_set
     and type transition_graph = TransitionGraph.t
end

(** For classical/non-probabilistic programs we want polynomial updates only. *)
module type ClassicalProgramModules = ProgramModules
  with module UpdateElement = Polynomials.Polynomial
