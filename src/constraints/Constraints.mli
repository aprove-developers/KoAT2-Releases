(** Provides default implementations of constraints. *)
open Batteries
open Atoms
   
(** Provides default implementations of constraints, i.e., a conjunction of atoms. *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials. *)
module ConstraintOver(A : ConstraintTypes.Atom) : ConstraintTypes.Constraint
       with type polynomial = A.polynomial
        and type value = A.value
        and type atom = A.t
        and type t = A.t list

(** Provides an implementation of constraints consisting of atoms over polynomials. *)
module Constraint :
sig
  include module type of ConstraintOver(Atom)

  (** Drops all nonlinear atoms from the constraints. Example: (a > 0 && b^2 < 2) gets transformed to (a > 0) *)
  val drop_nonlinear : t -> t
                
  (** The result of the following drop methods is not equivalent to the input constraint. 
      But each satisfying valuation of the input constraint is still a model of the new constraint. *)
    
  (** Returns the row of all coefficients of a variable in a constraint, i.e., used for farkas quantor elimination. *)
  val get_coefficient_vector : Var.t -> t -> value list
    
  (** Returns the matrix of all coefficients of a variable from a set of variables in a constraint, i.e., used for farkas quantor elimination. *)  
  val get_matrix : VarSet.t -> t -> value list list
    
  (** Returns the row of all constants in a constraint, i.e., used for farkas quantor elimination. *)
  val get_constant_vector : t -> value list
    
  (** TODO doc *)
  val dualise : Var.t list -> value list list -> polynomial list -> t
        
  (** TODO doc *)
  val max_of_occurring_constants : t -> OurInt.t

  (* Add operations specific to polynomial constraints here if needed *)
end

(** Provides an implementation of constraints consisting of atoms over parameter polynomials. *)
module ParameterConstraint :
sig
  include module type of ConstraintOver(ParameterAtom)

  (** Returns the row of all coefficients of a variable in a constraint, i.e., used for farkas quantor elimination. *)
  val get_coefficient_vector : Var.t -> t -> value list
    
  (** Returns the matrix of all coefficients of a variable from a set of variables in a constraint, i.e., used for farkas quantor elimination. *)  
  val get_matrix : VarSet.t -> t -> value list list
    
  (** Returns the row of all constants in a constraint, i.e., used for farkas quantor elimination. *)
  val get_constant_vector : t -> value list
  
  (** Applies Farkas-Transformation on a Constraint and a ParameterAtom. *)
  val farkas_transform : Constraint.t -> Atoms.ParameterAtom.t -> Constraint.t
    
  (* Add operations specific to parameter constraints here if needed *)
end

(** Provides an implementation of constraints consisting of atoms over bounds. *)
module BoundConstraint :
sig
  include module type of ConstraintOver(BoundAtom)
                       
  (* Add operations specific to parameter constraints here if needed *)
end