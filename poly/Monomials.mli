open Batteries
   
module MakeMonomial
         (Var : PolyTypes.ID)
         (Value : Number.Numeric)
       : PolyTypes.Monomial with module Var = Var
                             and module Value = Value
                             and module Valuation_ = Valuation.MakeValuation(Var)(Value)
                             and module RenameMap_ = RenameMap.MakeRenameMap(Var)
                             and type power = Powers.MakePower(Var)(Value).t
