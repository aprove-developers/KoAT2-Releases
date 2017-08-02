open Batteries
open PolyTypes
open ConstraintTypes

module Make(P : Polynomial) : Constraint with   
         module Atom_ = Atoms.Make(P)
