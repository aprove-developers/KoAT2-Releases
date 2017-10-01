open Batteries

(** Provides default implementations of a constraint *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials *)
module Make(A : ConstraintTypes.Atom) : ConstraintTypes.Constraint
       with type polynomial = A.polynomial
        and type value = A.value
        and type atom = A.t

module PolynomialConstraint :  ConstraintTypes.Constraint
       with type polynomial = Polynomial.t
        and type value = Polynomial.Value.t
        and type atom = Atoms.PolynomialAtom.t
        and module A = Atoms.PolynomialAtom
