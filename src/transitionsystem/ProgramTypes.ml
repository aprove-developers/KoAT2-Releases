open Batteries
   
(** Provides all module types related to the transition system (program graph) *)

(** A program is an integer transition system based on transitions and locations *)
module type Program =
  sig
    
    module Constraint_ : ConstraintTypes.Constraint
         
    (** A transition is an edge of a transition system.
        It connects two locations and is annotated with a guard and an update
        A guard has to be fulfiled for a state to reach another state via the transition
        An update assigns variables a new value as a linear combination of the old values *)
    module TransitionLabel :
    sig
      module Polynomial = Constraint_.Polynomial_
      module Var = Constraint_.Polynomial_.Var
      module Map : module type of Map.Make(Constraint_.Polynomial_.Var)
      type t
      exception RecursionNotSupported
      val mk : name:string ->
               start:string ->
               targets:(string * (Polynomial.t list)) list ->
               patterns:Var.t list ->
               guard:Constraint_.t ->
               vars:Var.t list ->
               t
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val start : t -> string
      val target : t -> string
      val update : t -> Var.t -> Polynomial.t Option.t
      val guard : t -> Constraint_.t
      val default : t
      val to_string : string -> string -> t -> string
    end
         
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

    module TransitionGraph : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(TransitionLabel)
                                  with type edge = Location.t * TransitionLabel.t * Location.t
                                   and type vertex = Location.t

    module Transition : module type of TransitionGraph.E
      
    module RV :
      sig
        type t = Transition.t * Constraint_.Polynomial_.Var.t
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val hash : t -> int
        val transition : t -> Transition.t
        val variable : t -> Constraint_.Polynomial_.Var.t
      end
         
    module RVG : module type of Graph.Persistent.Digraph.ConcreteBidirectional(RV)

    type t

    val add_vertices : TransitionGraph.t -> TransitionGraph.vertex list -> TransitionGraph.t

    val add_edges : TransitionGraph.t -> TransitionGraph.edge list -> TransitionGraph.t

    val mk : TransitionGraph.vertex list -> TransitionGraph.edge list -> TransitionGraph.t

    val from : Constraint_.Polynomial_.Var.t list
               -> TransitionLabel.t list
               -> Location.t
               -> t

    val rvg : t -> RVG.t

    val graph : t -> TransitionGraph.t

    (** Returns if the given transition is an initial transition. *)
    val is_initial : t -> Transition.t -> bool

    val to_string : t -> string

  end
