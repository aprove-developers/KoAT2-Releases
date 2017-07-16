open Batteries
open ID
open PolyTypes

module MakeMinMaxPolynomial
         (Var : ID)
         (Value : Number.Numeric)
       : MinMaxPolynomial with type var = Var.t
                           and type rename_map = Var.t Map.Make(Var).t
                           and type value = Value.t
                           and type valuation = Valuation.MakeValuation(Var)(Value).t
                           and type polynomial_ast = PolynomialAST(Var).t
