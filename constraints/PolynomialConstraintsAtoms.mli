open Batteries
open PolyTypes
open ConstraintTypes

module MakePolynomialConstraintsAtom(Var : ID) (Value : Number.Numeric) : PolynomialConstraintsAtom with   
         module Var = Var
       and module Value = Value
       and module Polynomial_ = Polynomials.MakePolynomial(Var)(Value)
