open Batteries
open Polynomials
open Bounds

module Automorphism : sig
  type t

  val to_string : t -> string
  val identity : t
  val apply_to_bound : Bound.t -> t Option.t -> Bound.t
end
