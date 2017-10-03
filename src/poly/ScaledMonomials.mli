open Batteries

(** Internal *)
module Make
         (Value : PolyTypes.Ring)
       : PolyTypes.ScaledMonomial with module Value = Value
                                   and type valuation = Valuation.Make(Value).t
                                   and type monomial = Monomials.Make(Value).t
