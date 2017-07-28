open Batteries
open PolyTypes
open ConstraintTypes

module MakePolynomialConstraints(P : Polynomial) : PolynomialConstraints with   
         module PolynomialConstraintsAtoms_ = PolynomialConstraintsAtoms.MakePolynomialConstraintsAtom(P)
