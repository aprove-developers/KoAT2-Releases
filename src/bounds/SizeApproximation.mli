open Batteries
open ProgramTypes
open RVGTypes
   
type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

type t

val empty : int -> t

val get : kind -> t -> Transition.t -> Var.t -> Bound.t

val add : kind -> Bound.t -> Transition.t -> Var.t -> t -> t

val add_all : kind -> Bound.t -> RV.t list -> t -> t

val to_string : ?html:bool -> t -> string

val equivalent : t -> t -> bool
