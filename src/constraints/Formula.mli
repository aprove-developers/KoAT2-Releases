open Batteries

(** Provides default implementations of a constraint *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials *)
module Make(C : ConstraintTypes.Constraint) : ConstraintTypes.Formula
       with type constr = C.t
        and type atom = C.atom
        and type polynomial = C.polynomial
        and type value = C.value
        and module C = C

module PolynomialFormula : ConstraintTypes.Formula
       with type constr = Constraints.PolynomialConstraint.t
        and type atom = Atoms.PolynomialAtom.t
        and type polynomial = Polynomial.t
        and type value = Polynomial.value
        and module C = Constraints.PolynomialConstraint
