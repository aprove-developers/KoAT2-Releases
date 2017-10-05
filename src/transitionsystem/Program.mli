open Batteries
   
(** Provides default modules to create locations, transitions and transitionsystems *)

(** A location is a node of a transition system and can be connected to other locations via transitions *)
module Location :
sig
  type t [@@deriving eq, ord]
  val hash : t -> int
  val to_string : t -> string
  val of_string : string -> t
end

module TransitionGraph : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(TransitionLabel)
                                      
module Transition : module type of TransitionGraph.E

module RV :
sig
  type t = Transition.t * Var.t [@@deriving eq]
  val compare : t -> t -> int
  val hash : t -> int
  val transition : t -> Transition.t
  val variable : t -> Var.t
end
     
module RVG :
sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectional(RV)

  (** Returns all the entry points of the SCC.
      Those are all result variables that are in the RVG, but not in the SCC and lead to any result variable in the RVG. *)
  val entry_points : t -> RV.t list -> RV.t Enum.t
end

type transition_set = Set.Make(Transition).t

type t

val add_vertices : TransitionGraph.t -> TransitionGraph.vertex list -> TransitionGraph.t

val add_edges : TransitionGraph.t -> TransitionGraph.edge list -> TransitionGraph.t

val mk : TransitionGraph.vertex list -> TransitionGraph.edge list -> TransitionGraph.t

val from : Var.t list
           -> TransitionLabel.t list
           -> Location.t
           -> t

val rvg : t -> RVG.t

val graph : t -> TransitionGraph.t

(** Returns a set of all transitions which occur directly before the given transition in the graph. 
       Corresponds to pre(t). *)
val pre : t -> Transition.t -> transition_set

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
val print_system : outdir:string -> file:string -> t -> unit

(** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the result variable graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
val print_rvg : outdir:string -> file:string -> t -> unit

(** Returns if the given transition is an initial transition. *)
val is_initial : t -> Transition.t -> bool

val to_string : t -> string
  
val vars : t -> VarSet.t
  
