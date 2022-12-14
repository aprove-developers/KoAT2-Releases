open Batteries

module Make(Num: PolyTypes.OurNumber):
  BoundType.Bound with type value = Num.t
                   and type polynomial = Polynomials.PolynomialOver(Num).t

module Bound: BoundType.Bound with type value = OurInt.t
                               and type polynomial = Polynomials.Polynomial.t

module RealBound: sig
  include BoundType.Bound with type value = OurFloat.t
                           and type polynomial = Polynomials.RealPolynomial.t

  val of_intbound: Bound.t -> t

  val of_intpoly: Polynomials.Polynomial.t -> t
end
