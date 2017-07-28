open Batteries
open PolyTypes
open ConstraintTypes

module MakeAtom(P : Polynomial) : Atom with   
         module Polynomial_ = P
