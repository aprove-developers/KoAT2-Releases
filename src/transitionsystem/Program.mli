open Batteries
open Polynomials
open Constraints
   
(** Provides default modules to create locations, transitions and transitionsystems *)

open ProgramTypes
open RVGTypes
   
type t

val add_locations : Location.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

val add_transitions : Transition.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

(** Removes the location from the program and all edges to it. *)
val remove_location : t -> Location.t -> t

val remove_transition : t -> Transition.t -> t

val map_graph : (TransitionGraph.t -> TransitionGraph.t) -> t -> t
  
val mk : Transition.t Enum.t -> TransitionGraph.t

val from : TransitionLabel.t list -> Location.t -> t

val rvg : [`Lower | `Upper] -> t -> RVG.t

val graph : t -> TransitionGraph.t

(** Adds the invariant to the location of the program. *)
val add_invariant : Location.t -> Constraint.t -> t -> t

(** Returns a set of all transitions which occur directly before the given transition in the graph. 
       Corresponds to pre(t). *)
val pre : t -> Transition.t -> Transition.t Enum.t

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
val print_system : label:(TransitionLabel.t -> string) -> outdir:Fpath.t -> file:string -> t -> unit

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the result variable graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
val print_rvg : [`Lower | `Upper] -> label:(RV.t -> string) -> outdir:Fpath.t -> file:string -> t -> unit

(** Returns if the given transition is an initial transition. *)
val is_initial : t -> Transition.t -> bool

(** Returns if the given transition is an initial transition. *)
val is_initial_location : t -> Location.t -> bool

val equivalent : t -> t -> bool
  
val to_string : t -> string

val to_simple_string : t -> string
  
val vars : t -> VarSet.t

val transitions : t -> TransitionSet.t
  
(** Returns all locations which occur in the transitions, but each location only once. *)
val locations : Transition.t Enum.t -> Location.t Enum.t
  
val start : t -> Location.t
  
val sccs : t -> TransitionSet.t Enum.t

(** Returns all transitions, that belong to an SCC. *)
val non_trivial_transitions : t -> TransitionSet.t
