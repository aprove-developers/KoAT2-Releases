open OurBase
(** Provides default implementation of a monomial. *)

module MakeOverIndeterminate (I : PolyTypes.Indeterminate) (Value : PolyTypes.Ring) :
  PolyTypes.Monomial
    with type value = Value.t
     and type valuation = Valuation.MakeOverIndeterminate(I)(Value).t
     and type indeterminate = I.t

(** Constructs a default monomial using a list of pairs of variables and their exponents. *)
module Make (Value : PolyTypes.Ring) :
  module type of MakeOverIndeterminate (VarIndeterminate) (Value) with type indeterminate = Var.t
