(** Modul handles labels of classical transitions. *)
open OurBase
(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update.
    A guard has to be fulfiled for a state to reach another state via the transition.
    An update assigns variables a new value as a linear combination of the old values. *)

include ProgramTypes.TransitionLabel

type update_element = Polynomials.Polynomial.t

(** TODO doc? *)
val mk : id:int option ->
         cost:Polynomials.Polynomial.t ->
         assignments: Polynomials.Polynomial.t list ->
         patterns:Var.t list ->
         guard:Guard.t ->
         t

(** Appends the second label to the first label.
    An evaluation of the resulting label is equivalent to an evaluation of the first label and then the second label. *)
val append : t -> t -> t

(** Returns the update map of the transitionlabel *)
val update_map : t -> Polynomials.Polynomial.t ProgramTypes.var_map

(** Returns the update of a variable. *)
val update : t -> Var.t -> Polynomials.Polynomial.t Option.t

(** Assign a fresh id to the transition *)
val fresh_id : t -> t

(** We execute CFRefinement with guard && invariant -> We need to separate invariant afterwards. *)
val separate_guard_invariant : t -> Invariant.t -> t

val vars_without_memoization : t -> VarSet.t

(** Sets costs to 1, and guard and invariant to true *)
val only_update: t -> t

(** Returns the number of variables. *)
val input_size : t -> int

val has_tmp_vars : t -> bool

(** Returns a string representing the left hand side of the update function.
    Can be used to dump the Program to a file. *)
val update_to_file_string_lhs: t -> string

(** Returns a string representing the right hand side of the update function.
    Can be used to dump the Program to a file. *)
val update_to_file_string_rhs: t -> string

val eliminate_tmp_var: Var.t -> t -> MaybeChanged.status * t
