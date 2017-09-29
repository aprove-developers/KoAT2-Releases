open Batteries
open PolyTypes

(** Provides default implementations of a constraint *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials *)
module Make(P : Polynomial) : ConstraintTypes.Formula
       with module Polynomial_ = P
        and module Atom_ = Atoms.Make(P)
        and module Constraint_ = Constraints.Make(P)
