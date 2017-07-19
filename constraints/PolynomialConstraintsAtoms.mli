open Batteries
open PolyTypes
open ConstraintTypes

module MakePolynomialConstraintsAtom(Var : ID) (Value : Number.Numeric) : PolynomialConstraintsAtom with   
    type var = Var.t
    and type polynomial = Polynomials.MakePolynomial(Var)(Value).t
    and type rename_map = Var.t Map.Make(Var).t
    and type value = Value.t
    and type valuation = Valuation.MakeValuation(Var)(Value).t