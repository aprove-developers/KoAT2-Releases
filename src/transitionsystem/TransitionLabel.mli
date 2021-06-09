(** Modul handles labels of transitions. *)
open Batteries
(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update.
    A guard has to be fulfiled for a state to reach another state via the transition.
    An update assigns variables a new value as a linear combination of the old values. *)

(** Module representing the guard. *)
module Guard = Constraints.Constraint

(** Type as a short form of our polynomials over [OurInt]. *)
type polynomial = Polynomials.Polynomial.t

(** Module representing a map from variables to variables. *)
module VarMap : module type of Map.Make(Var)

(** Kind of bound, i.e., lower or upper. Is the ever used?? *)
type kind = [ `Lower | `Upper ]  [@@deriving eq, ord]

(** A transition label consists of an unique id, an update function, a guard and a cost function. *)
type t

(** KoAT2 does not support recursion yet. *)
exception RecursionNotSupported

(** Creates a label from an update function and a guard, a cost can be set, too (the default is the constant function one).
The string is used to set "Com_1". TODO doc ? *)
val make : ?cost:polynomial -> string -> update:polynomial VarMap.t -> guard:Guard.t -> t

(** TODO doc? *)
val mk : ?cost:polynomial ->
         com_kind:string ->
         targets:(string * (polynomial list)) list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         t

val fresh_id : t -> t

val normalise : t -> VarSet.t -> t

val trival : VarSet.t -> t

(** Appends the second label to the first label.
    An evaluation of the resulting label is equivalent to an evaluation of the first label and then the second label. *)
val append : t -> t -> t

(** Returns if the two labels are the same entity. *)
val same : t -> t -> bool

(** Returns if the two labels describe the same transition *)
val equivalent : t -> t -> bool

(** TODO doc *)
val compare_same : t -> t -> int

(** TODO doc *)
val compare_equivalent : t -> t -> int

(** Returns the unique id. *)
val id : t -> int

(** Returns the update of a variable. *)
val update : t -> Var.t -> polynomial Option.t

(** Returns the guard of the label. *)
val guard : t -> Guard.t

(** Returns a new transition label with the guard changed. *)
val map_guard : (Guard.t -> Guard.t) -> t -> t

(** Returns a default label with id 0, [true] as the guard,no update function and the default cost function. *)
val default : t

(** Returns the set of variables. *)
val vars : t -> VarSet.t

val vars_update : t -> VarSet.t

(** Returns the set of input variables of the transition, i.e. the non temporary variables  *)
val input_vars : t -> VarSet.t

(** Returns the number of variables. *)
val input_size : t -> int

(** Returns the cost function *)
val cost : t -> polynomial

(** Returns a string representing the label. *)
val to_string : t -> string

(** Returns a string representing the left hand side of the update function. Parameter {i to_file} is used to get a representation with less special characters. *)
val update_to_string_lhs : ?to_file:bool -> t -> string

(** Returns a string representing the right hand side of the update function. Parameter {i to_file} is used to get a representation with less special characters. *)
val update_to_string_rhs : ?to_file:bool -> t -> string

(** Returns a string representing the guard. Parameter {i to_file} is used to get a representation with less special characters. *)
val guard_to_string : ?to_file:bool -> t -> string

(** Returns a string representing the cost. Parameter {i to_file} is used to get a representation with less special characters. *)
val cost_to_string : ?to_file:bool -> t -> string

(** Returns a string representing the id of the label. *)
val to_id_string : t -> string

(** TODO doc *)
val rename : Var.t list -> t -> t

val rename2 : RenameMap.t -> t -> t

val remove_non_contributors : VarSet.t -> t -> t