open Batteries
   
(** Provides all module types related to the transition system (program graph) *)

(** A location is a node of a transition system and can be connected to other locations via transitions *)
module type Location =
  sig
    type t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val of_string : string -> t
  end

(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update
    A guard has to be fulfiled for a state to reach another state via the transition
    An update assigns variables a new value as a linear combination of the old values *)
module type Transition =
  sig
    module Constraint_ : ConstraintTypes.Constraint
    module Map : module type of Map.Make(Constraint_.Atom_.Polynomial_.Var)
    type t
    exception RecursionNotSupported
    val mk : name:string ->
             start:string ->
             targets:(string * (Constraint_.Atom_.Polynomial_.t list)) list ->
             patterns:Constraint_.Atom_.Polynomial_.Var.t list ->
             guard:Constraint_.t ->
             vars:Constraint_.Atom_.Polynomial_.Var.t list ->
             t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val start : t -> string
    val target : t -> string
    val update : t -> Constraint_.Atom_.Polynomial_.t Map.t
    val guard : t -> Constraint_.t
    val default : t
    val to_string : string -> string -> t -> string
  end

(** A program is an integer transition system based on transitions and locations *)
module type Program =
  sig
    module Transition_ : Transition
    module Location_ : Location
    module TransitionGraph : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location_)(Transition_)
                                  with type edge = Location_.t * Transition_.t * Location_.t
                                   and type vertex = Location_.t

    module RV :
      sig
        type t = TransitionGraph.E.t * Transition_.Constraint_.Atom_.Polynomial_.Var.t
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val hash : t -> int
        val transition : t -> (Location_.t * Transition_.t * Location_.t)
        val variable : t -> Transition_.Constraint_.Atom_.Polynomial_.Var.t
      end
    module RVG : module type of Graph.Persistent.Digraph.ConcreteBidirectional(RV)

    type t

    val add_vertices : TransitionGraph.t -> TransitionGraph.vertex list -> TransitionGraph.t

    val add_edges : TransitionGraph.t -> TransitionGraph.edge list -> TransitionGraph.t

    val mk : TransitionGraph.vertex list -> TransitionGraph.edge list -> TransitionGraph.t

    val from : Transition_.Constraint_.Atom_.Polynomial_.Var.t list
               -> Transition_.t list
               -> Location_.t
               -> t

    val create_variable_graph : t -> RVG.t

    val graph : t -> TransitionGraph.t

    (** Returns if the given transition is an initial transition. *)
    val is_initial : t -> Transition_.t -> bool

    val to_string : t -> string

  end
