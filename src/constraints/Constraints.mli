open Batteries
open Atoms
   
(** Provides default implementations of a constraint *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials *)
module ConstraintOver(A : ConstraintTypes.Atom) : ConstraintTypes.Constraint
       with type polynomial = A.polynomial
        and type value = A.value
        and type atom = A.t

module Constraint :
sig
  include module type of ConstraintOver(Atom)

  (* Add operations specific to polynomial constraints here if needed *)
end

module ParameterConstraint :
sig
  include module type of ConstraintOver(ParameterAtom)

  (* Add operations specific to parameter constraints here if needed *)
end
