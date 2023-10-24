open Batteries

type t

val to_string : t -> string
val identity : t
val apply_to_bound : Bounds.Bound.t -> t Option.t -> Bounds.Bound.t
