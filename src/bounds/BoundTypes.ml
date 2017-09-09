open Batteries

(** Provides all module types related to bounds *)

(** An atom is a comparison between two polynomials *)
module type Approximation =
  sig
    module Program_ : TransitionGraphTypes.Program

    module Bound : module type of MinMaxPolynomial.Make
                                    (Program_.Transition_.Constraint_.Atom_.Polynomial_.Var)
                                    (Program_.Transition_.Constraint_.Atom_.Polynomial_.Value)
         
    type t
         
    (** Distinguish between lower and upper bounds *)
    type kind = Lower | Upper

                      
    (** Returns an empty approximation that does not contain any non-trivial information.
        That means, that every upper bound is infinite and every lower bound is minus infinite.
        The first parameter should be the count of transitions in the program.
        The second parameter should be the count of program variables. *)
    val empty : int -> int -> t

      
    (** Timebound related methods *)
                        
    (** Returns a timebound of the specified kind for the transition. *)
    val timebound : kind -> t -> Program_.Transition_.t -> Bound.t

    (** Returns a timebound of the specified kind for the execution of the whole graph. *)
    val timebound_graph : kind -> t -> Program_.t -> Bound.t

    (** Adds the information that the specified bound is a valid timebound for the given transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
    val add_timebound : kind -> Bound.t -> Program_.Transition_.t -> t -> t
      

    (** Sizebound related methods *)

    (** Returns a sizebound of the specified kind for the var of the transition. 
        A sizebound is expressed in relation to the input variable values of the program. *)
    val sizebound : kind -> t -> Program_.Transition_.t -> Program_.Transition_.Constraint_.Atom_.Polynomial_.Var.t -> Bound.t

    (** Returns a local sizebound of the specified kind for the var of the transition. 
        A local sizebound is expressed in relation to the values directly before executing the transition. *)
    val sizebound_local : kind -> t -> Program_.Transition_.t -> Program_.Transition_.Constraint_.Atom_.Polynomial_.Var.t -> Bound.t

    (** Adds the information that the specified bound is a valid sizebound for the given variable of the transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
    val add_sizebound : kind -> Bound.t -> Program_.Transition_.t -> Program_.Transition_.Constraint_.Atom_.Polynomial_.Var.t -> t -> t

  end

module type TimeBounds =
  sig
    module Approximation_ : Approximation

    (** Performs a single improvement step to find better timebounds for the approximation and updates the approximation. *)
    val improve : Approximation_.Program_.t -> Approximation_.t -> Approximation_.t
  end
  
module type SizeBounds =
  sig
    module Approximation_ : Approximation

    (** Performs a single improvement step to find better sizebounds for the approximation and updates the approximation. *)
    val improve : Approximation_.Program_.t -> Approximation_.t -> Approximation_.t
  end
