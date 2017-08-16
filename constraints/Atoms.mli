open Batteries
open PolyTypes
open ConstraintTypes

(** Provides default implementations of an atom *)

(** Constructs a default constraint using a simple record *)
module Make(P : Polynomial) : Atom with   
         module Polynomial_ = P
