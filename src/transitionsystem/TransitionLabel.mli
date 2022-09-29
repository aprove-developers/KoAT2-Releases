(** Modul handles labels of transitions. *)
open Batteries
(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update.
    A guard has to be fulfiled for a state to reach another state via the transition.
    An update assigns variables a new value as a linear combination of the old values. *)

(** Module representing the guard. *)
module Guard = Constraints.Constraint

(** Module representing the invariants. *)
module Invariant = Constraints.Constraint

(** Type as a short form of our polynomials over [OurInt]. *)
type polynomial = Polynomials.Polynomial.t

(** Module representing a map from variables to variables. *)
module VarMap : module type of Map.Make(Var)

(** A transition label consists of an unique id, an update function, a guard and a cost function. *)
type t


(** TODO doc? *)
val mk : cost:polynomial ->
         assignments: polynomial list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         vars:Var.t list ->
         t

val fresh_id : t -> t

val add_invariant : t -> Invariant.t -> t

val normalise : t -> VarSet.t -> t

(** Appends the second label to the first label.
    An evaluation of the resulting label is equivalent to an evaluation of the first label and then the second label. *)
val append : t -> t -> t

(** Simplifies the guard of the transition. This function performs calls to the SMT solver and is therefore expensive.
 * This method makes O(n^2) calls to the smt solver where n is the number of atoms in the guard. *)
val simplify_guard : t -> t

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

(** Overapproximates nonlinear updates by nondeterministic updates. Useful for Farkas lemma *)
val overapprox_nonlinear_updates : t -> t

(** Returns the update map of the transitionlabel *)
val update_map : t -> polynomial VarMap.t

(** Returns the guard of the label. *)
val guard : t -> Guard.t

(** Returns the guard of the label without considering invariants. *)
val guard_without_inv : t -> Guard.t

val without_inv : t -> t

(** Returns the invariant. *)
val invariant : t -> Invariant.t

(** Returns a new transition label with the guard changed. *)
val map_guard : (Guard.t -> Guard.t) -> t -> t

(** Returns a default label with id 0, [true] as the guard,no update function and the default cost function. *)
val default : t

(** Returns the set of variables. *)
val vars : t -> VarSet.t

val vars_without_memoization : t -> VarSet.t

val vars_update : t -> VarSet.t

(** Returns the set of input variables of the transition, i.e. the non temporary variables  *)
val input_vars : t -> VarSet.t

(** Returns the number of variables. *)
val input_size : t -> int

(** Returns the cost function *)
val cost : t -> polynomial

(** Returns t with costs 1 *)
val only_update : t -> t

(** Returns a string representing the label. *)
val to_string : ?pretty:bool -> t -> string

(** Returns a string representing the left hand side of the update function. Parameter {i to_file} is used to get a representation with less special characters. *)
val update_to_string_lhs : ?to_file:bool -> t -> string

(** Returns a string representing the right hand side of the update function. Parameter {i to_file} is used to get a representation with less special characters. *)
val update_to_string_rhs : ?to_file:bool -> t -> string

val update_to_string_lhs_pretty : t -> string

val update_to_string_rhs_pretty : t -> string

(** Returns a string representing the guard. Parameter {i to_file} is used to get a representation with less special characters. *)
val guard_to_string : ?to_file:bool -> ?pretty:bool -> t -> string

(** Returns a string representing the cost. Parameter {i to_file} is used to get a representation with less special characters. *)
val cost_to_string : ?to_file:bool -> t -> string

(** Returns a string representing the id of the label. *)
val to_id_string : t -> string

(** The call {i fill_up_arg_vars_up_to_num n} adds trivial updates for the first {i n}, i.e. Arg_0, .., Arg_n-1 arguments that are not contained in the labels update map *)
val fill_up_arg_vars_up_to_num: int -> t -> t

(** TODO doc *)
val rename : RenameMap.t -> t -> t

(** Rename temporary variables to identifiers provided by the (possibly infinite) lazy list *)
val rename_temp_vars : t -> Var.t LazyList.t -> t

val remove_non_contributors : VarSet.t -> t -> t

(** We execute CFRefinement with guard && invariant -> We need to separate invariant afterwards. *)
val separate_guard_invariant : t -> Invariant.t -> t
