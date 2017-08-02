open Batteries
open ID
open PolyTypes

module Make
         (Var : ID)
         (Value : Number.Numeric)
       : Valuation with type var = Var.t
                    and type value = Value.t
