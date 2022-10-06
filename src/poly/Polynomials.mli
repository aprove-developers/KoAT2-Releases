(** Provides default implementations of polynomials. *)
open Batteries
open PolyTypes

(** Provides default implementations of polynomials. *)

(** Constructs a default polynomial using a list of monomials and their coefficients *)
module PolynomialOver
         (Value : Ring)
       : Polynomial with type value = Value.t
                     and type valuation = Valuation.Make(Value).t
                     and type monomial = Monomials.Make(Value).t
                     and type scaled_monomial = ScaledMonomials.Make(Value).t

(** Provides default implementation of polynomials ranged over [OurInt]. *)
module Polynomial :
sig
  (** Provides default implementation of polynomials ranged over [OurInt]. *)

  include module type of PolynomialOver(OurInt)

  (** Separates polynomial into a two list. One list with all scaled monomials of the polynomial with negative coefficients and the other list with only scaled monomials with non-negative coefficients. *)
  val separate_by_sign : t -> (t * t)

  (** TODO doc *)
  val max_of_occurring_constants : t -> OurInt.t

end

(** Provides default implementation of polynomials where the coefficients are polynomials over [OurInt]. *)
module ParameterPolynomial :
sig
  (** Provides default implementation of polynomials where the coefficients are polynomials over [OurInt]. *)
  include module type of PolynomialOver(PolynomialOver(OurInt))

  (** Evaluates coefficients (which are polynomials over [OurInt]) and returns the resulting polynomial over [OurInt].*)
  val eval_coefficients : (Var.t -> OurInt.t) -> PolynomialOver(PolynomialOver(OurInt)).t -> PolynomialOver(OurInt).t

  (** Transforms the template polynomial such that all inner values get lifted to the outer polynomial.
  Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)
  val flatten : t -> PolynomialOver(OurInt).t

  (** Lifts a polynomial to a parameter polynomial such that the inner structure is kept.
  Example: 2x +3 is interpreted as 2x+3 and not as the constant polynomial (2x+3)*(1)*)
  val of_polynomial : PolynomialOver(OurInt).t -> t
end

module RealPolynomial :
sig
  include module type of PolynomialOver(OurFloat)

  val separate_by_sign : t -> (t * t)

  val max_of_occurring_constants : t -> OurFloat.t
  val of_intpoly : Polynomial.t -> t
  val to_intpoly : t -> Polynomial.t
  val degree_coeff_list : t -> value list
end

module RationalPolynomial :
sig
  include module type of PolynomialOver(OurRational)

  val degree_coeff_list : t -> value list

  (** Multiply with lcm *)
  val normalize : t -> Polynomial.t

  (** Multiply with lcm and return lcm*)
  val normalize_return_factor : t -> Polynomial.t * (OurInt.t * OurInt.t)

  (** Returns poly where each coeff. is replaced by its absolute, ceiled value *)
  val overapprox : t -> Polynomial.t

  val of_intpoly : Polynomial.t -> t

  val is_integer_poly : t -> bool

end

module RealParameterPolynomial :
sig
  include module type of PolynomialOver(PolynomialOver(OurFloat))

  val eval_coefficients : (Var.t -> OurFloat.t) -> PolynomialOver(PolynomialOver(OurFloat)).t -> PolynomialOver(OurFloat).t

  val flatten : t -> PolynomialOver(OurFloat).t

  val of_polynomial : PolynomialOver(OurFloat).t -> t

  val of_intpoly : Polynomial.t -> t

  val of_int_parapoly : PolynomialOver(PolynomialOver(OurInt)).t -> t

  val to_int_parapoly : t -> PolynomialOver(PolynomialOver(OurInt)).t
end
