open Batteries

(** Internal *)
module Make
         (Value : PolyTypes.Ring)
       : PolyTypes.ScaledMonomial with type value = Value.t
                                   and type valuation = Valuation.Make(Value).t
                                   and type monomial = Monomials.Make(Value).t
