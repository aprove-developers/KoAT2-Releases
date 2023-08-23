open! OurBase

module Make (Num : PolyTypes.OurNumber) :
  BoundType.Bound with type value = Num.t and type polynomial = Polynomials.PolynomialOver(Num).t

module Bound :
  BoundType.Bound
    with type value = OurInt.t
     and type polynomial = Polynomials.Polynomial.t
     and type t = Make(OurInt).t

module RationalBound : sig
  include
    BoundType.Bound
      with type value = OurRational.t
       and type polynomial = Polynomials.RationalPolynomial.t
       and type t = Make(OurRational).t

  val of_intbound : Bound.t -> t
  val of_intpoly : Polynomials.Polynomial.t -> t

  val to_intbound : t -> Bound.t
  (** Ceil Floats to Ints *)
end
