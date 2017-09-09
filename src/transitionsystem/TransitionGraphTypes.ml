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

(** A result variable graph relates variables of transitions to the variables in other transitions which value they can influence.
    It can be derived from a transitionsystem and is needed to recognize SCCs of variables which can influence each other. *)
module type VariableGraph =
  sig
    module ResultVariable :
      sig
        module Transition_ : Transition
        module Location_ : Location
        type t = (Location_.t * Transition_.t * Location_.t) * Transition_.Constraint_.Atom_.Polynomial_.Var.t
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val hash : t -> int
        val transition : t -> (Location_.t * Transition_.t * Location_.t)
        val variable : t -> Transition_.Constraint_.Atom_.Polynomial_.Var.t
      end
  
    type t

    module Graph : sig
      include module type of Graph.Persistent.Digraph.ConcreteBidirectional(ResultVariable)
      val add_vertices : t -> vertex list -> t
      val add_edges : t -> edge list -> t
      val mk : vertex list -> edge list -> t
    end

    val graph : t -> Graph.t
         
  end

(** A graph is a integer transition system based on transitions and locations *)
module type TransitionGraph =
  sig
    module Transition_ : Transition
    module Location_ : Location
    module VariableGraph_ : VariableGraph
           with module ResultVariable.Transition_ = Transition_
            and module ResultVariable.Location_ = Location_
    module Graph : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location_)(Transition_)
                                  with type edge = Location_.t * Transition_.t * Location_.t
                                   and type vertex = Location_.t

    type t

    val add_vertices : Graph.t -> Graph.vertex list -> Graph.t

    val add_edges : Graph.t -> Graph.edge list -> Graph.t

    val mk : Graph.vertex list -> Graph.edge list -> Graph.t

    val from : Transition_.Constraint_.Atom_.Polynomial_.Var.t list
               -> Transition_.t list
               -> Location_.t
               -> t

    val create_variable_graph : t -> VariableGraph_.t

    val graph : t -> Graph.t

    (** Returns if the given transition is an initial transition. *)
    val is_initial : t -> Transition_.t -> bool

    val to_string : t -> string

  end
