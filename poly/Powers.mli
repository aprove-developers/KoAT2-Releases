open Batteries

module MakePower
         (Var : PolyTypes.ID)
         (Value : Number.Numeric)
       : PolyTypes.Power with module Var = Var
                          and module Value = Value
                          and module Valuation_ = Valuation.MakeValuation(Var)(Value)
                          and module RenameMap_ = RenameMap.MakeRenameMap(Var)
