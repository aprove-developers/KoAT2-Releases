open Batteries

module MakeScaledMonomial
         (Var : PolyTypes.ID)
         (Value : Number.Numeric)
       : PolyTypes.ScaledMonomial with module Var = Var
                         and module Value = Value
                         and module Valuation_ = Valuation.MakeValuation(Var)(Value)
                         and module RenameMap_ = RenameMap.MakeRenameMap(Var)
                         and type power = Powers.MakePower(Var)(Value).t
                         and type monomial = Monomials.MakeMonomial(Var)(Value).t

