open Batteries
open Polynomials
open Constraints

(** Provides default modules to create locations, transitions and transitionsystems *)

open ProgramTypes

type t

val add_locations : Location.t Enum.t -> t -> t

val add_transitions : Transition.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

(** Removes the location from the program and all edges to it. *)
val remove_location : t -> Location.t -> t

val remove_transition : t -> Transition.t -> t

val map_graph : (TransitionGraph.t -> TransitionGraph.t) -> t -> t

val mk : Transition.t Enum.t -> TransitionGraph.t

val rename : t -> t

val from : Transition.t list -> Location.t -> t

(**  Like from but expects a string for the start location. This function will then automatically
     determine the start location's arity *)
val from_startstr: Transition.t list -> string -> t

val graph : t -> TransitionGraph.t

val invariant: Location.t -> t -> Constraint.t

(** Adds the invariant to the location of the program. *)
val add_invariant : Location.t -> Constraint.t -> t -> t

type pre_cache
val new_cache : unit -> pre_cache
(** Returns a set of all transitions which occur directly before the given transition in the graph.
       Corresponds to pre(t). Makes use of Caching, since it has to call the SMT solver and is hence ressource intensive *)
val pre : pre_cache -> t -> Transition.t -> Transition.t Enum.t

(** Returns a set of all general transitions which contain a transition occuring directly before the given transition in the graph
 *)
val pre_gt : pre_cache -> t -> GeneralTransition.t -> GeneralTransitionSet.t

(** Returns if the given transition is an initial transition. *)
val is_initial : t -> Transition.t -> bool

(** Returns if the given general transition is an initial transition *)
val is_initial_gt : t -> GeneralTransition.t -> bool

(** Returns if the given transition is an initial transition. *)
val is_initial_location : t -> Location.t -> bool

val equivalent : t -> t -> bool

val to_formatted_string: t -> FormattedString.t

val to_string : t -> string

val to_simple_string : show_gtcost:bool -> t -> string

val vars : t -> VarSet.t

val input_vars : t -> VarSet.t

val transitions : t -> TransitionSet.t

(** Returns all locations which occur in the transitions, but each location only once. *)
val locations : Transition.t Enum.t -> Location.t Enum.t

val locations_of_program: t -> LocationSet.t

val start : t -> Location.t

val sccs : t -> TransitionSet.t Enum.t

(**  Similar to sccs but returns all sccs not only the biggest sccs. I.e. the powerset of every element of [sccs prog] is it self a
     a subset of [all_sccs prog] *)
val all_sccs : t -> TransitionSet.t Enum.t

(** Returns all transitions, that belong to an SCC. *)
val non_trivial_transitions : t -> TransitionSet.t

val generalized_transitions : t -> GeneralTransitionSet.t
