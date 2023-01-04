(** Provides an implementation of polynomial-exponential expressions*)
open Batteries
open Polynomials
open ProgramModules
open Formulas
open BoundsInst

module ConstantConstraint :
sig
    module Comparator :
    sig
        type t = EQ | NEQ

    end

    type atom = (Comparator.t * OurInt.t)

    type t = C of atom list | T | F

    val to_string : t -> string
end

module PE :
sig
    type t = (ConstantConstraint.t * RationalPolynomial.t * int * int) list

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
