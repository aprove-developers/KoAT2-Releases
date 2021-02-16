open Batteries
open Polynomials

exception Invalid_input of String.t

module ProbUpdate :
  sig
    type t
    val from: OurNum.t -> Polynomial.t list -> t
    val to_string: t -> string
    val probability: t -> OurNum.t
    val update: t -> Polynomial.t list
  end

module ExactProgram :
  sig
    type t
    val variables: t -> Var.t list
    val guardpoly: t -> Polynomial.t
    val guardvalue: t -> OurInt.t
    val updates: t -> ProbUpdate.t list
    val directtermination: t -> ProbUpdate.t option
    val precision: t -> OurInt.t option
    val from: Var.t list -> Polynomial.t -> OurInt.t -> ProbUpdate.t list -> ProbUpdate.t option -> OurInt.t option -> (OurInt.t list) option -> t
    val to_string: t -> string
    (** Check if the given exact program is valid. This includes the right variables were used, the probabilities are valid and the polynomial vectors all have the same length. *)
    val is_valid: t -> bool
    val is_cp_program: t -> bool
    val to_python: t -> string
  end

