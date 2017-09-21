open Batteries
open PolyTypes
open ConstraintTypes

(** Provides default implementations of a constraint *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials *)
module Make(P : Polynomial) : Formula
       with module Polynomial_ = P
       and module Atom_ = Atoms.Make(P)
