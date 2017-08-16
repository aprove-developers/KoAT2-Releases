open Batteries
open PolyTypes
open ConstraintTypes

(** TODO Internal *)
module Make(P : Polynomial) : Atom with   
         module Polynomial_ = P
