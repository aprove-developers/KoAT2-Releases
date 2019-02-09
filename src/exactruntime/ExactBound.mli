open Batteries
open Polynomials

type t

(* val infinity : t *)
val const : OurNum.t -> t
val var : Var.t -> t
val neg : t -> t
val pow : OurNum.t -> t -> t
val sum : t -> t -> t
val list_sum : t list -> t
val prod : t -> t -> t
val list_prod : t list -> t
val cos : t -> t
val sin : t -> t