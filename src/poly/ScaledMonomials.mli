open Batteries

(** Internal *)
module Make
         (Value : PolyTypes.Ring)
       : PolyTypes.ScaledMonomial with module Value = Value
                                   and module Valuation_ = Valuation.Make(Value)
                                   and type monomial = Monomials.Make(Value).t
