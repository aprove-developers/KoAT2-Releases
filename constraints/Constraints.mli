open Batteries
open PolyTypes
open ConstraintTypes

module MakeConstraint(P : Polynomial) : Constraint with   
         module Atom_ = Atoms.MakeAtom(P)
