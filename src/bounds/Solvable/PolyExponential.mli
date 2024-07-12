open Batteries
open Bounds

(** Provides an implementation of polynomial-exponential expressions*)

module ConstantConstraint : sig
  module Comparator : sig
    type t = EQ | NEQ
  end

  type atom = Comparator.t * OurInt.t
  type t = C of atom list | T | F

  val mk : OurInt.t -> Comparator.t -> t
  val mk_eq : OurInt.t -> t
  val mk_neq : OurInt.t -> t
  val equal : t -> t -> bool
  val to_string : t -> string
end

module type IntSupRing = sig
  type t

  include PolyTypes.Math with type t := t
  include PolyTypes.Ring with type t := t

  val of_ourint : OurInt.t -> t
  val div : t -> t -> t
end

module PE (Value : IntSupRing) : sig
  module Polynomial : module type of Polynomials.PolynomialOver (Value)

  type t = (ConstantConstraint.t * Polynomial.t * int * Value.t) list

  val mk : ConstantConstraint.t -> Polynomial.t -> int -> Value.t -> t
  val mk_cons : Value.t -> t
  val mk_var : Var.t -> t
  val to_string : t -> string
  val to_string_pretty : t -> string
  val substitute : (Var.t, t) Hashtbl.t -> Polynomial.t -> t
  val substitute_f : (Var.t -> Polynomial.t) -> t -> t
  val normalize : t list -> t list
  val max_const : t -> OurInt.t
  val compute_closed_form : (Var.t * Polynomial.t) list -> t list
  val add : t -> t -> t
  val mul : t -> t -> t
  val power : t -> int -> t
end

module RationalPE : sig
  include module type of PE (OurRational)

  val remove_frac : t -> t
  val overapprox : t -> Bound.t -> Bound.t
end

module ComplexPE : sig
  include module type of PE (OurAlgebraicComplex)

  val to_bound : t -> Bound.t
  (** Adds absolute values and ceiling and returns a bound in the program variables and the distinguished variable n. *)
end
