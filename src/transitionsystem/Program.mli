open Batteries
open Polynomials
open Constraints
   
(** Provides default modules to create locations, transitions and transitionsystems *)

open ProgramTypes
   
type t

val add_locations : Location.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

val add_transitions : Transition.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

(** Removes the location from the program and all edges to it. *)
val remove_location : t -> Location.t -> t

val remove_transition : t -> Transition.t -> t

val map_graph : (TransitionGraph.t -> TransitionGraph.t) -> t -> t
  
val mk : Transition.t Enum.t -> TransitionGraph.t

val rename : t -> t
  
val from : Transition.t list -> Location.t -> t

val graph : t -> TransitionGraph.t

(** Adds the invariant to the location of the program. *)
val add_invariant : Location.t -> Constraint.t -> t -> t

(** Returns a set of all transitions which occur directly before the given transition in the graph. 
       Corresponds to pre(t). *)
val pre : t -> Transition.t -> Transition.t Enum.t

(** Returns if the given transition is an initial transition. *)
val is_initial : t -> Transition.t -> bool

(** Returns if the given transition is an initial transition. *)
val is_initial_location : t -> Location.t -> bool

val equivalent : t -> t -> bool
  
val to_string : ?html:bool -> t -> string

val to_simple_string : t -> string
  
val vars : t -> VarSet.t

val input_vars : t -> VarSet.t


val transitions : t -> TransitionSet.t
  
(** Returns all locations which occur in the transitions, but each location only once. *)
val locations : Transition.t Enum.t -> Location.t Enum.t
  
val start : t -> Location.t
  
val sccs : t -> TransitionSet.t Enum.t

(** Returns all transitions, that belong to an SCC. *)
val non_trivial_transitions : t -> TransitionSet.t
