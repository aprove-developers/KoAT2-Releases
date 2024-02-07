open! OurBase
(** Provides default implementations of atoms. *)

open Polynomials

(** Provides default implementations of atoms, i.e., for two objects q and p (e.g. polynomials, bounds), and a comparator ~, the expression  q ~ p is an atom. *)

(** Provides an implementation of atoms over polynomials. *)
module Atom : sig
  include
    ConstraintTypes.Atom
      with type monomial = Polynomial.monomial
       and type polynomial = Polynomial.t
       and type value = Polynomial.value

  val max_of_occurring_constants : t -> OurInt.t
  (** TODO doc*)

  val get_constant : t -> OurInt.t
  (** Returns the single right hand side constant of the atom. Only works as intended for comparison with <= or integer atoms *)

  val remove_strict : t -> t

  val poly : t -> Polynomial.t
  (** Returns p for an atom p ≤ 0*)

  val poly_lt : t -> Polynomial.t
  (** Returns p for an atom p < 0, i.e., p - 1 for p ≤ 0*)
end

module RationalAtom : sig
  include
    ConstraintTypes.Atom
      with type polynomial = RationalPolynomial.t
       and type value = RationalPolynomial.value
       and type monomial = RationalPolynomial.monomial

  val is_linear : t -> bool
  (** Returns if both polynomials are linear. *)

  val get_coefficient : monomial -> t -> value
  (** Returns the coefficient of a monomial which is normalised to the lhs. *)

  val max_of_occurring_constants : t -> OurRational.t

  (* Add operations specific to polynomial atoms here if needed *)
  val of_intatom : Atom.t -> t
end

module ParameterAtom : sig
  include
    ConstraintTypes.Atom
      with type monomial = ParameterPolynomial.monomial
       and type polynomial = ParameterPolynomial.t
       and type value = ParameterPolynomial.value
end

module RationalParameterAtom : sig
  include
    ConstraintTypes.Atom
      with type monomial = RationalParameterPolynomial.monomial
       and type polynomial = RationalParameterPolynomial.t
       and type value = RationalParameterPolynomial.value

  val replace_nonlinear_monomials_with_temp_vars : t -> t
end
