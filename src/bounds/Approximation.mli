open Batteries

(** Provides default implementations of an approximation *)

(*module Make(P : ProgramTypes.Program) : BoundTypes.Approximation with   
         module Program_ = P*)
    module Program_ = Program

    module Transition = Program_.Transition
    module Value = Program_.Polynomial_.Value
                                
    module Bound : module type of MinMaxPolynomial.Make(Program_.Polynomial_)

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
    val timebound : kind -> t -> Transition.t -> Bound.t

    (** Returns a timebound of the specified kind for the execution of the whole graph. *)
    val timebound_graph : kind -> t -> Program_.t -> Bound.t

    (** Adds the information that the specified bound is a valid timebound for the given transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
    val add_timebound : kind -> Bound.t -> Transition.t -> t -> t
      

    (** Sizebound related methods *)

    (** Returns a sizebound of the specified kind for the var of the transition. 
        A sizebound is expressed in relation to the input variable values of the program. *)
    val sizebound : kind -> t -> Transition.t -> Var.t -> Bound.t

    (** Adds the information that the specified bound is a valid sizebound for the given variable of the transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
    val add_sizebound : kind -> Bound.t -> Transition.t -> Var.t -> t -> t