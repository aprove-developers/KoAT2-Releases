open Batteries
(** Provides an implementation of polynomial-exponential expressions*)

open Polynomials
open ProgramModules
open Formulas
open Bounds

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

module PE : sig
  type t = (ConstantConstraint.t * RationalPolynomial.t * int * int) list

  val mk : ConstantConstraint.t -> RationalPolynomial.t -> int -> int -> t
  val to_string : t -> string
  val to_string_pretty : t -> string
  val substitute : (Var.t, t) Hashtbl.t -> Polynomial.t -> t
  val normalize : t list -> t list
  val max_const : t -> OurInt.t
  val remove_frac : t -> t
  val compute_closed_form : (Var.t * Polynomial.t) list -> t list

  (* val monotonic_kernel : Formula.t -> Formula.t -> t -> t * ((int * int) * (int * int)) list *)

  val overapprox : t -> Bound.t -> Bound.t
end
