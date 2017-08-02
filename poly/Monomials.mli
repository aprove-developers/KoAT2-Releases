open Batteries
   
module Make
         (Var : PolyTypes.ID)
         (Value : Number.Numeric)
       : PolyTypes.Monomial with module Var = Var
                             and module Value = Value
                             and module Valuation_ = Valuation.Make(Var)(Value)
                             and module RenameMap_ = RenameMap.Make(Var)
                             and type power = Powers.Make(Var)(Value).t
