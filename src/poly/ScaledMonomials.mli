(** Provides default implementation of a scaled monomial, i.e., a finite product of powers (e.g.: 5xy^2, y but not 5xy^2 + 7). *)
open Batteries

module MakeOverIndeterminate(I: PolyTypes.Indeterminate)(Value: PolyTypes.Ring):
  PolyTypes.ScaledMonomial
    with type value = Value.t
    and  type indeterminate = I.t
    and  type valuation = Valuation.MakeOverIndeterminate(I)(Value).t
    and  type monomial = Monomials.MakeOverIndeterminate(I)(Value).t

module Make(Value : PolyTypes.Ring):
  module type of MakeOverIndeterminate(VarIndeterminate)(Value)
      with type value = Value.t
      and  type indeterminate = Var.t
      and  type valuation = Valuation.Make(Value).t
      and  type monomial = Monomials.Make(Value).t
