open Batteries
open ID
open PolyTypes

module MakePolynomial
         (Var : ID)
         (Value : Number.Numeric)
       : Polynomial with module Var = Var
                     and module Value = Value
                     and module Valuation_ = Valuation.MakeValuation(Var)(Value)
                     and module RenameMap_ = RenameMap.MakeRenameMap(Var)
                     and type power = Powers.MakePower(Var)(Value).t
                     and type monomial = Monomials.MakeMonomial(Var)(Value).t
                     and type scaled_monomial = ScaledMonomials.MakeScaledMonomial(Var)(Value).t
