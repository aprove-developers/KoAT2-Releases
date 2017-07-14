open Batteries
open ID
open PolyTypes
   
module MakeVariableTerm
         (Var : ID)
         (Value : Number.Numeric)
       : Evaluable with type var = Var.t
                    and type t = Var.t
                    and type rename_map = Var.t Map.Make(Var).t
                    and type value = Value.t
                    and type valuation = Valuation.MakeValuation(Var)(Value).t
