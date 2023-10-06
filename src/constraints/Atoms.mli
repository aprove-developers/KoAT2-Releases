open! OurBase
(** Provides default implementations of atoms. *)

open Polynomials

(** Provides default implementations of atoms, i.e., for two objects q and p (e.g. polynomials, bounds), and a comparator ~, the expression  q ~ p is an atom. *)

(** Constructs a default constraint using a simple record *)
module AtomOver (P : ConstraintTypes.Atomizable) :
  ConstraintTypes.Atom with type polynomial = P.t and type value = P.value and module P = P
(** Provides default implementations of atoms, i.e., for two objects q and p (e.g. polynomials, bounds), and a comparator ~, the expression q ~ p is an atom. *)

(** Provides an implementation of atoms over polynomials. *)
module Atom : sig
  include
    ConstraintTypes.Atom
      with type polynomial = Polynomial.t
       and type value = Polynomial.value
       and module P = Polynomial
       and type t = AtomOver(PolynomialOver(OurInt)).t

  val max_of_occurring_constants : t -> OurInt.t
  (** TODO doc*)

  val get_constant : t -> value
  (** Returns the single right hand side constant of the atom. Only works as intended for comparison with <= or integer atoms *)

  (* Only works as intended for integer atoms *)
  val remove_strict : t -> t

  (* Add operations specific to polynomial atoms here if needed *)
end

module RationalAtom : sig
  include
    ConstraintTypes.Atom
      with type polynomial = RationalPolynomial.t
       and type value = RationalPolynomial.value
       and module P = RationalPolynomial
       and type t = AtomOver(PolynomialOver(OurRational)).t

  val is_linear : t -> bool
  (** Returns if both polynomials are linear. *)

  val get_coefficient : Var.t -> t -> value
  (** Returns the coefficient of a variable which is normalised to the lhs. *)

  val max_of_occurring_constants : t -> OurRational.t

  (* Add operations specific to polynomial atoms here if needed *)
  val of_intatom : Atom.t -> t
end

(** Provides an implementation of atoms over parameter polynomials. *)
module ParameterAtomOver (Value : PolyTypes.Ring) : sig
  include
    ConstraintTypes.Atom
      with type polynomial = ParameterPolynomialOver(Value).t
       and type value = ParameterPolynomialOver(Value).value
       and module P = ParameterPolynomialOver(Value)
       and type t = AtomOver(ParameterPolynomialOver(Value)).t
end

module ParameterAtom : sig
  include
    ConstraintTypes.Atom
      with type polynomial = ParameterPolynomial.t
       and type value = ParameterPolynomial.value
       and module P = ParameterPolynomial
       and type t = AtomOver(ParameterPolynomialOver(OurInt)).t
end

module RationalParameterAtom : sig
  include
    ConstraintTypes.Atom
      with type polynomial = RationalParameterPolynomial.t
       and type value = RationalParameterPolynomial.value
       and module P = RationalParameterPolynomial
       and type t = AtomOver(ParameterPolynomialOver(OurRational)).t

  val replace_nonlinear_monomials_with_temp_vars : t -> t
end
