open Batteries
open PolyTypes
open ConstraintTypes

(** Holds default implementations of a constraint *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials *)
module Make(P : Polynomial) : Constraint with   
         module Atom_ = Atoms.Make(P)
