open Batteries
open ID
open PolyTypes

module MakePower
         (Var : ID)
         (Value : Number.Numeric)
       : Power with type var = Var.t
                and type value = Value.t
                and type rename_map = Var.t Map.Make(Var).t
                and type valuation = Valuation.MakeValuation(Var)(Value).t
