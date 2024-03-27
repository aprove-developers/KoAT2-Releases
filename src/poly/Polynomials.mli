open! OurBase
(** Provides default implementations of polynomials. *)

open PolyTypes

type ('indeterminate, 'value, 'indeterminate_cmp_wit) generic_polynomial
(** generic type of polynomials *)

type 'value generic_var_polynomial = (Var.t, 'value, Var.comparator_witness) generic_polynomial
(** generic type of Polynomials with variable indeterminates *)

(** Provides default implementations of (laurent) polynomials. *)
module PolynomialOverIndeterminate (I : Indeterminate) (Value : Ring) :
  Polynomial
    with type value = Value.t
     and type valuation = Valuation.MakeOverIndeterminate(I)(Value).t
     and type monomial = (I.t, I.comparator_witness) Monomials.generic_monomial
     and type scaled_monomial = ScaledMonomials.MakeOverIndeterminate(I)(Value).t
     and type indeterminate = I.t
     and type t = (I.t, Value.t, I.comparator_witness) generic_polynomial

(** Constructs a default (laurent) polynomial using a list of monomials and their coefficients *)
module PolynomialOver (Value : Ring) : sig
  include
    Polynomial
      with type value = Value.t
       and type valuation = Valuation.Make(Value).t
       and type monomial = (Var.t, Var.comparator_witness) Monomials.generic_monomial
       and type scaled_monomial = ScaledMonomials.Make(Value).t
       and type indeterminate = Var.t
       and type t = Value.t generic_var_polynomial

  val substitute_all : (Var.t, t, Var.comparator_witness) Map.t -> t -> t
  (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
      Leaves all variables unchanged which are not in the replacement map.  *)
end

(** Provides default implementation of polynomials ranged over [OurInt]. *)
module Polynomial : sig
  (** Provides default implementation of polynomials ranged over [OurInt]. *)

  include module type of PolynomialOver (OurInt)

  val primitive_part_content_factorisation : t -> OurInt.t * t
  (** The factorisation is such that the content is never negative *)

  val separate_by_sign : t -> t * t
  (** Separates polynomial into a two list. One list with all scaled monomials of the polynomial with negative coefficients and the other list with only scaled monomials with non-negative coefficients. *)

  val roots : t -> OurAlgebraicComplex.t list

  val max_of_occurring_constants : t -> OurInt.t
  (** TODO doc *)
end

(** Provides default implementation of polynomials ranged over [OurRational]. *)
module RationalPolynomial : sig
  include module type of PolynomialOver (OurRational)

  val separate_by_sign : t -> t * t
  val of_intconstant : OurInt.t -> t
  val max_of_occurring_constants : t -> OurRational.t
  val of_intpoly : Polynomial.t -> t
  val degree_coeff_list : t -> value list

  val normalize : t -> Polynomial.t
  (** Multiply with lcm. *)

  val normalize_return_factor : t -> Polynomial.t * OurRational.t
  (** Multiply with lcm and return lcm. *)

  val overapprox : t -> Polynomial.t
  (** Returns poly where each coeff. is replaced by its absolute, ceiled value *)

  val is_integral : t -> bool
end

module RationalLaurentPolynomial : module type of PolynomialOver (OurRational)
(** Provides default implementation of laurent polynomials ranged over [OurRational]. *)

(** Provides polynomials where the coefficients are polynomials over {i Value}. *)
module ParameterPolynomialOver (Value : PolyTypes.Ring) : sig
  include module type of PolynomialOver (PolynomialOver (Value))

  val eval_coefficients :
    (Var.t -> Value.t) -> PolynomialOver(PolynomialOver(Value)).t -> PolynomialOver(Value).t
  (** Evaluates coefficients (which are polynomials over {i Value}) and returns the resulting polynomial over {i Value}.*)

  val flatten : t -> PolynomialOver(Value).t
  (** Transforms the template polynomial such that all inner values get lifted to the outer polynomial.
  Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)

  val of_polynomial : PolynomialOver(Value).t -> t
  (** Lifts a polynomial to a parameter polynomial such that the inner structure is kept.
  Example: 2x +3 is interpreted as 2x+3 and not as the constant polynomial (2x+3)*(1)*)
end

module ParameterPolynomial : module type of ParameterPolynomialOver (OurInt)
(** Provides default implementation of polynomials where the coefficients are polynomials over [OurInt]. *)

module RationalParameterPolynomial : module type of ParameterPolynomialOver (OurRational)
(** Provides default implementation of polynomials where the coefficients are polynomials over [OurRational]. *)
