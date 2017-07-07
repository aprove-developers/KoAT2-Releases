open ID

module type Valuation =
  sig
    type t
    type var
    type value = Big_int.big_int
    val from : (var * value) list -> t
    val zero : var list -> t
    val eval : var -> t -> value
    val vars : t -> var list
  end

module type ValuationFunctor =
  functor (Id : ID) -> Valuation with type var = Id.t

module MakeValuation(Id : ID) : Valuation with type var = Id.t

module StringValuation : Valuation with type var = StringID.t
