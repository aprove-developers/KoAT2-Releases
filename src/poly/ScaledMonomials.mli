(** Provides default implementation of a scaled monomial. *)
open Batteries

(** Provides default implementation of a scaled monomial, i.e., a finite product of powers (e.g.: 5xy^2, y but not 5xy^2 + 7). *)
(** Internal *)
module Make
         (Value : PolyTypes.Ring)
       : PolyTypes.ScaledMonomial with type value = Value.t
                                   and type valuation = Valuation.Make(Value).t
                                   and type monomial = Monomials.Make(Value).t
