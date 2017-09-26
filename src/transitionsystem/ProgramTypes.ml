open Batteries
   
(** Provides all module types related to the transition system (program graph) *)

(** A program is an integer transition system based on transitions and locations *)
module type Program =
  sig
    
    module Polynomial_ : PolyTypes.Polynomial
    module Atom_ : ConstraintTypes.Atom with module Polynomial_ = Polynomial_
    module Constraint_ : ConstraintTypes.Constraint with module Polynomial_ = Polynomial_
    module Formula_ : ConstraintTypes.Formula with module Polynomial_ = Polynomial_
         
    (** A transition is an edge of a transition system.
        It connects two locations and is annotated with a guard and an update
        A guard has to be fulfiled for a state to reach another state via the transition
        An update assigns variables a new value as a linear combination of the old values *)
    module TransitionLabel :
    sig
      module Polynomial = Constraint_.Polynomial_
      module Map : module type of Map.Make(Var)
      module Bound : module type of MinMaxPolynomial.Make(Polynomial) 
      type kind = Lower | Upper  [@@deriving eq, ord]
      type t
      exception RecursionNotSupported
      val make : name:string -> start:string -> target:string -> update:Polynomial.t Map.t -> guard:Constraint_.t -> t
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
      (** Returns a local sizebound of the specified kind for the var of the transition. 
          A local sizebound is expressed in relation to the values directly before executing the transition. *)
      val sizebound_local : kind -> t -> Var.t -> Bound.t
      val to_string : t -> string
    end
         
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
         
    module RVG : module type of Graph.Persistent.Digraph.ConcreteBidirectional(RV)

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
    val pre : t -> Transition.t -> Transition.t Set.t

    (** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
    val print_system : outdir:string -> file:string -> t -> unit

    (** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the result variable graph of the program. 
        For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
    val print_rvg : outdir:string -> file:string -> t -> unit

    (** Returns if the given transition is an initial transition. *)
    val is_initial : t -> Transition.t -> bool

    val to_string : t -> string
    
    val vars : t -> Var.t Set.t

  end
