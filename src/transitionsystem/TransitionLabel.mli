open Batteries

(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update
    A guard has to be fulfiled for a state to reach another state via the transition
    An update assigns variables a new value as a linear combination of the old values *)
module Guard = Constraints.Constraint
type polynomial = Polynomials.Polynomial.t
module VarMap : module type of Map.Make(Var)
                          
type kind = [ `Lower | `Upper ]  [@@deriving eq, ord]

type t

exception RecursionNotSupported

val make : ?cost:polynomial -> string -> update:polynomial VarMap.t -> guard:Guard.t -> t

val mk : ?cost:polynomial ->
         com_kind:string ->
         targets:(string * (polynomial list)) list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         t
         
val make_prob : ?cost:polynomial -> string -> updates:(polynomial VarMap.t) list -> guard:Guard.t -> probabilities:(float list) -> t list

(*val mk_prob : ?cost:polynomial ->
         probabilities:float ->
         com_kind:string ->
         targets:(string * (polynomial list)) list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         [t]*)

(** Appends the second label to the first label.
    An evaluation of the resulting label is equivalent to an evaluation of the first label and then the second label. *)
val append : t -> t -> t

(** Returns if the two labels are the same entity. *)
val same : t -> t -> bool

(** Returns if the two labels describe the same transition *)
val equivalent : t -> t -> bool
  
val compare_same : t -> t -> int

val compare_equivalent : t -> t -> int

val id : t -> int

val update : t -> Var.t -> polynomial Option.t

val guard : t -> Guard.t

val probability : t -> float

(** Returns a new transition label with the guard changed. *)
val map_guard : (Guard.t -> Guard.t) -> t -> t

val default : t

val vars : t -> VarSet.t
  
val cost : t -> polynomial

val to_string : t -> string

val to_id_string : t -> string
