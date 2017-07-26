open Batteries
open PolyTypes
open ConstraintTypes

module MakePolynomialConstraints(Var : ID) (Value : Number.Numeric) : PolynomialConstraints with   
         module Var = Var
       and module Value = Value
       and module Polynomial_ = Polynomials.MakePolynomial(Var)(Value)
       and module PolynomialConstraintsAtoms_ = PolynomialConstraintsAtoms.MakePolynomialConstraintsAtom(Var)(Value)