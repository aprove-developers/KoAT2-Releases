open Batteries

(** Provides default implementations of an atom *)

(** Constructs a default constraint using a simple record *)
module Make(P : PolyTypes.Polynomial) : ConstraintTypes.Atom
       with type polynomial = P.t
        and type value = P.value
        and module P = P

module PolynomialAtom : ConstraintTypes.Atom
       with type polynomial = Polynomial.t
        and type value = Polynomial.value
