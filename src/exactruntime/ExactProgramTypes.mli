open Batteries
open Polynomials

exception Invalid_input of String.t

module ProbUpdate :
  sig
    type t
    val from: OurNum.t -> OurInt.t list -> t
    val to_string: t -> string
    val probability: t -> OurNum.t
    val update: t -> OurInt.t list
  end

module ExactProgram :
  sig
    type t
    val guardvector: t -> OurInt.t list
    val guardvalue: t -> OurInt.t
    val updates: t -> ProbUpdate.t list
    val directtermination: t -> ProbUpdate.t option
    val precision: t -> OurInt.t option
    val from: OurInt.t list -> OurInt.t -> ProbUpdate.t list -> ProbUpdate.t option -> OurInt.t option -> (OurInt.t list) option -> t
    val to_string: t -> string
    val is_valid: ?logger:Logger.log -> t -> bool
    val to_sage: t -> string
  end

