open Batteries
open ID
open PolyTypes

module MakeValuation
         (Var : ID)
         (Value : Number.Numeric)
       : Valuation with type var = Var.t
                    and type value = Value.t
