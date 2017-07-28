open Batteries
open PolyTypes
open ConstraintTypes

module MakePolynomialConstraintsAtom(P : Polynomial) : PolynomialConstraintsAtom with   
         module Polynomial_ = P
