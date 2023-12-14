open! OurBase
(** Provides default implementation of a monomial.
    When used in Laurent polynomials, the exponents may be negative. *)

module MakeOverIndeterminate (I : PolyTypes.Indeterminate) (Value : PolyTypes.Ring) :
  PolyTypes.Monomial
    with type value = Value.t
     and type valuation = Valuation.MakeOverIndeterminate(I)(Value).t
     and type indeterminate = I.t

(** Constructs a default monomial using a list of pairs of variables and their exponents.
    When used in Laurent polynomials, the exponents may be negative. *)
module Make (Value : PolyTypes.Ring) :
  PolyTypes.Monomial
    with type value = Value.t
     and type valuation = Valuation.MakeOverIndeterminate(VarIndeterminate)(Value).t
     and type indeterminate = Var.t
     and type t = MakeOverIndeterminate(VarIndeterminate)(Value).t
