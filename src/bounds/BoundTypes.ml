open Batteries

(** Provides all module types related to bounds *)

(** An atom is a comparison between two polynomials *)
module type Approximation =
  sig
    module TransitionGraph_ : TransitionGraphTypes.TransitionGraph

    type t
         
    (** The actual type we use to represent bounds *)
    type bound

    (** Distinguish between lower and upper bounds *)
    type kind = Lower | Upper

                      
    (** Returns an empty approximation that does not contain any non-trivial information.
        That means, that every upper bound is infinite and every lower bound is minus infinite.  *)
    val empty : t

      
    (** Timebound related methods *)
                        
    (** Returns a timebound of the specified kind for the transition. *)
    val timebound : kind -> t -> TransitionGraph_.Transition_.t -> bound

    (** Returns a timebound of the specified kind for the execution of the whole graph. *)
    val timebound_graph : kind -> t -> TransitionGraph_.t -> bound

    (** Adds the information that the specified bound is a valid timebound for the given transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
    val add_timebound : kind -> bound -> TransitionGraph_.Transition_.t -> t -> t
      

    (** Sizebound related methods *)

    (** Returns a sizebound of the specified kind for the var of the transition. 
        A sizebound is expressed in relation to the input variable values of the program. *)
    val sizebound : kind -> t -> TransitionGraph_.Transition_.t -> TransitionGraph_.Transition_.Constraint_.Atom_.Polynomial_.Var.t -> bound

    (** Returns a local sizebound of the specified kind for the var of the transition. 
        A local sizebound is expressed in relation to the values directly before executing the transition. *)
    val sizebound_local : kind -> t -> TransitionGraph_.Transition_.t -> TransitionGraph_.Transition_.Constraint_.Atom_.Polynomial_.Var.t -> bound

    (** Adds the information that the specified bound is a valid sizebound for the given variable of the transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
    val add_sizebound : kind -> bound -> TransitionGraph_.Transition_.t -> TransitionGraph_.Transition_.Constraint_.Atom_.Polynomial_.Var.t -> t -> t

  end
