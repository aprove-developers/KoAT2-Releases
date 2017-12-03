open Batteries
open Polynomials
   
(** Provides default modules to create locations, transitions and transitionsystems *)

module Types :
sig  
  (** A location is a node of a transition system and can be connected to other locations via transitions *)
  module Location :
  sig
    type t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val of_string : string -> t
  end
  module LocationSet : module type of Set.Make(Location)

  module Transition :
  sig
    type t = Location.t * TransitionLabel.t * Location.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_id_string : t -> string
    val to_string : t -> string
    val src : t -> Location.t
    val label : t -> TransitionLabel.t
    val target : t -> Location.t
    val cost : t -> Polynomial.t
  end
  module TransitionSet : module type of Set.Make(Transition)

  module TransitionGraph :
  sig
    include module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(TransitionLabel)
    val locations : t -> LocationSet.t
    val transitions : t -> TransitionSet.t
    val loc_transitions : t -> Location.t list -> TransitionSet.t
    val equal : t -> t -> bool
  end
       
  module RV :
  sig
    type t = Transition.t * Var.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_id_string : t -> string
    val to_string : t -> string
    val hash : t -> int
    val transition : t -> Transition.t
    val variable : t -> Var.t
  end
       
  module RVG :
  sig
    include module type of Graph.Persistent.Digraph.ConcreteBidirectional(RV)

    val rvs_to_id_string : RV.t list -> string

    val rvs_to_string : RV.t list -> string

    val pre : t -> RV.t -> RV.t Enum.t

    (** Returns all the entry points of the SCC.
      Those are all result variables that are in the RVG, but not in the SCC and lead to any result variable in the RVG. *)
    val entry_points : t -> RV.t list -> RV.t Enum.t

    (** Returns all transitions that are used in the SCC of the RVG. *)
    val transitions : RV.t list -> Transition.t Enum.t
      
  end
end

open Types
     
type t

val add_locations : Location.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

val add_transitions : Transition.t Enum.t -> TransitionGraph.t -> TransitionGraph.t

(** Removes the location from the program and all edges to it. *)
val remove_location : t -> Location.t -> t

val remove_transition : t -> Transition.t -> t

val map_graph : (TransitionGraph.t -> TransitionGraph.t) -> t -> t
  
val mk : Transition.t Enum.t -> TransitionGraph.t

val from : TransitionLabel.t list -> Location.t -> t

val rvg : t -> RVG.t

val graph : t -> TransitionGraph.t

(** Returns a set of all transitions which occur directly before the given transition in the graph. 
       Corresponds to pre(t). *)
val pre : t -> Transition.t -> Transition.t Enum.t

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
val print_system : label:(TransitionLabel.t -> string) -> outdir:Fpath.t -> file:string -> t -> unit

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the result variable graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
val print_rvg : label:(RV.t -> string) -> outdir:Fpath.t -> file:string -> t -> unit

(** Returns if the given transition is an initial transition. *)
val is_initial : t -> Transition.t -> bool

(** Returns if the given transition is an initial transition. *)
val is_initial_location : t -> Location.t -> bool

val equal : t -> t -> bool
  
val to_string : t -> string
  
val vars : t -> VarSet.t

(** Returns all locations which occur in the transitions, but each location only once. *)
val locations : Transition.t Enum.t -> Location.t Enum.t
  
val start : t -> Location.t
  
