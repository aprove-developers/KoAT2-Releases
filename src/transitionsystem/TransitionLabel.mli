open Batteries

(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update
    A guard has to be fulfiled for a state to reach another state via the transition
    An update assigns variables a new value as a linear combination of the old values *)
module Guard : module type of Constraints.Make(Polynomials.Make(PolyTypes.OurInt))
module Polynomial : module type of Polynomials.Make(PolyTypes.OurInt)
module Map : module type of Map.Make(Var)
                          
type kind = Lower | Upper  [@@deriving eq, ord]

type t

exception RecursionNotSupported

val make : name:string -> start:string -> target:string -> update:Polynomial.t Map.t -> guard:Guard.t -> t

val mk : name:string ->
         start:string ->
         targets:(string * (Polynomial.t list)) list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         t

val equal : t -> t -> bool

val compare : t -> t -> int

val start : t -> string

val target : t -> string

val update : t -> Var.t -> Polynomial.t Option.t

val guard : t -> Guard.t

val default : t

val to_string : t -> string
