open Batteries

module Automorphism (Bound : BoundType.Bound) : sig
  type t

  val to_string : t -> string
  val identity : t
  val apply_to_bound : Bound.t -> t Option.t -> Bound.t
end
