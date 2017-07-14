open Batteries
open ID
open PolyTypes

module MakePolynomial
         (Var : ID)
         (Value : Number.Numeric)
       : Polynomial with type var = Var.t
                     and type rename_map = Var.t Map.Make(Var).t
                     and type value = Value.t
                     and type valuation = Valuation.MakeValuation(Var)(Value).t
                     and type power = Powers.MakePower(Var)(Value).t
                     and type monomial = Monomials.MakeMonomial(Var)(Value).t
                     and type scaled_monomial = ScaledMonomials.MakeScaledMonomial(Var)(Value).t
