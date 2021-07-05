(** Provides default module to handle programs. *)
open Batteries
open Polynomials
open Constraints
(** Provides default module to handle programs. *)

open ProgramTypes

(** Type of a program consisting of a program graph and a start location. *)
type t

(** Adds all locations from an enum to a transtion graph. *)
val add_locations : Location.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

(** Adds all transitions from an enum to a transtion graph. *)
val add_transitions : Transition.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

(** Removes the location from the program and all edges to it. *)
val remove_location : t -> Location.t -> t

(** Removes a transition from a program. *)
val remove_transition : t -> Transition.t -> t

(* Removes the transitions from a certain transitionset to a program *)
val remove_TransitionSet: ProgramTypes.TransitionSet.t -> t -> t


(** TODO doc *)
val map_graph : (TransitionGraph.t -> TransitionGraph.t) -> t -> t

(** Creates a transition graph from an enum of transitions. *)
val mk : Transition.t Enum.t -> TransitionGraph.t

(** TODO doc *)
val rename : t -> t

(** Creates a program from a list of transitions and a (start) location. *)
val from : Transition.t list -> Location.t -> t

(** Returns transition graph of a program. *)
val graph : t -> TransitionGraph.t

(** Adds the invariant to a location of the program. *)
val add_invariant : Location.t -> Constraint.t -> t -> t

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
val is_initial_location : t -> Location.t -> bool

(** Returns true if the program graphs are equivalent and both start locations are equal. *)
val equivalent : t -> t -> bool

(** Returns a string representing the program. *)
val to_string : ?to_file:bool -> t -> string

(** Input is not interpreted as a filepath, but as a program in simple mode. Method returns a string representation of a program from such an input. *)
val to_simple_string : t -> string

(** Returns all variables of the program. *)
val vars : t -> VarSet.t

(** Returns all input variables of the program. *)
val input_vars : t -> VarSet.t

(** Returns the number of variables. *)
val cardinal_vars : t -> int

(** Returns a set of all transitions which occur in the program graph of the program. *)
val transitions : t -> TransitionSet.t

(** Returns all locations which occur in the transitions, but each location only once. *)
val locations : Transition.t Enum.t -> Location.t Enum.t

(** Returns start location. *)
val start : t -> Location.t

(** Returns the (biggest) strongly connected components of the transiton graph. *)
val sccs : t -> TransitionSet.t Enum.t

(** Returns the number of transition involved in a scc. *)
val cardinal_trans_scc : t -> int

(** Returns all transitions which are parallel to a given transition. Thus, all transitions start in the same location and end in the same location. *)
val parallelTransitions : t -> Location.t * TransitionLabel.t * Location.t -> TransitionSet.t

(** Returns all transitions, that belong to an SCC. *)
val non_trivial_transitions : t -> TransitionSet.t

(** Creates a file (if it does not already exist) and writes the program into it. *)
val to_file : t -> string -> unit

(** Computes all entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
val entry_transitions : Batteries.Logger.log -> t -> ProgramTypes.Transition.t list -> ProgramTypes.Transition.t Batteries.List.t

(** Computes all outgoing transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
val outgoing_transitions : Batteries.Logger.log -> t -> ProgramTypes.Transition.t list -> ProgramTypes.Transition.t Batteries.List.t
