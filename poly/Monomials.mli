open Batteries
open ID
open PolyTypes
   
module MakeMonomial
         (Var : ID)
         (Value : Number.Numeric)
       : Monomial with type var = Var.t
                   and type value = Value.t
                   and type rename_map = Var.t Map.Make(Var).t
                   and type valuation = Valuation.MakeValuation(Var)(Value).t
                   and type power = Powers.MakePower(Var)(Value).t
