open! OurBase

module type PolyAdapter = sig
  include PolyTypes.Polynomial

  val of_poly : Polynomials.Polynomial.t -> t
end
