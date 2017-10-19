open Batteries

(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update
    A guard has to be fulfiled for a state to reach another state via the transition
    An update assigns variables a new value as a linear combination of the old values *)
module Guard = Constraints.Constraint
type polynomial = Polynomials.Polynomial.t
module Map : module type of Map.Make(Var)
                          
type kind = [ `Lower | `Upper ]  [@@deriving eq, ord]

type t

exception RecursionNotSupported

val make : ?cost:polynomial -> string -> start:string -> target:string -> update:polynomial Map.t -> guard:Guard.t -> t

val mk : ?cost:polynomial ->
         name:string ->
         start:string ->
         targets:(string * (polynomial list)) list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         t

val equal : t -> t -> bool

val compare : t -> t -> int

val start : t -> string

val target : t -> string

val update : t -> Var.t -> polynomial Option.t

val guard : t -> Guard.t

val default : t

val cost : t -> polynomial

val to_string : t -> string
