open Batteries
open PolyTypes
open ConstraintTypes

module Make(P : Polynomial) : Atom with   
         module Polynomial_ = P
