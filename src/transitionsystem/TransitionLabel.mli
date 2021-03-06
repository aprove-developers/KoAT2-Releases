open Batteries
open Polynomials
open BoundsInst

(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update
    A guard has to be fulfiled for a state to reach another state via the transition
    An update assigns variables a new value as a linear combination of the old values *)
module Guard = Constraints.Constraint
module VarMap : module type of Map.Make(Var)

type kind = [ `Lower | `Upper ]  [@@deriving eq, ord]

type trans_id_counter

val new_trans_id_counter: unit -> trans_id_counter

type t

val get_unique_gt_id: trans_id_counter -> unit -> int

exception RecursionNotSupported

module UpdateElement :
  sig
    type t = Poly of Polynomials.Polynomial.t | Dist of ProbDistribution.t [@@deriving eq,ord]
    val to_string : t -> string
    val to_short_string : t -> string
    val vars : Var.t -> t -> VarSet.t
    val is_polynomial : t -> bool

    val mk_identity : Var.t -> t
  end

val make : trans_id_counter -> ?cvect:(Polynomial.t * RealBound.t)
        -> string -> input_vars_ordered:Var.t list -> update:UpdateElement.t VarMap.t -> update_vars_ordered:Var.t List.t -> guard:Guard.t -> t

val mk : trans_id_counter -> ?cvect:(Polynomial.t * RealBound.t) ->
         com_kind:string ->
         targets:(string * (UpdateElement.t list)) list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         t

val make_prob : trans_id_counter -> ?cvect:(Polynomial.t * RealBound.t) -> string
             -> input_vars_ordered:Var.t list -> update:UpdateElement.t VarMap.t -> update_vars_ordered:Var.t List.t -> guard:Guard.t
             -> gt_id:int -> probability:OurFloat.t -> t

val mk_prob : trans_id_counter -> ?cvect:(Polynomial.t * RealBound.t) ->
         com_kind:string ->
         targets:(string * (UpdateElement.t list)) list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         gt_id:int ->
         probability:OurFloat.t ->
         t

(** Appends the second label to the first label.
    An evaluation of the resulting label is equivalent to an evaluation of the first label and then the second label.
    Only works when no variable get sampled from a distribution during the update of the first transition *)
val append : trans_id_counter -> new_gt_id:int -> t -> t -> t

(** Returns a guard which constraints the possibility for the second label beeing evaluated in sequence with the first one *)
val append_guard : t -> t -> Guard.t

(** Returns true iff the two labels are the same entity. *)
val same : t -> t -> bool

(** Returns true iff the two labels belong to the same general transition *)
val same_gt : t -> t -> bool

val update_cost : (Polynomial.t * RealBound.t) -> t -> t

(** Returns if the two labels describe the same transition *)
val equivalent : t -> t -> bool

val compare_same : t -> t -> int

val compare_equivalent : t -> t -> int

val id : t -> int
val gt_id : t -> int

val update : t -> Var.t -> UpdateElement.t Option.t
val update_map : t -> UpdateElement.t VarMap.t

val guard : t -> Guard.t
val guard_without_invariants: t -> Guard.t
val invariants: t -> Guard.t

val probability : t -> OurFloat.t

(* Adds an invariant to a transition Label *)
val add_invariant: Guard.t -> t -> t

val default : t

val vars : t -> VarSet.t

(** Returns the set of input variables of the transition, i.e. the non temporary variables  *)
val input_vars : t -> VarSet.t

val input_size : t -> int

val cost : t -> Polynomial.t

val gtcost : t -> RealBound.t

val to_string : t -> string

val update_to_string_lhs : t -> string

val update_to_string_rhs : t -> string

val guard_to_string : t -> string

val to_id_string : t -> string
