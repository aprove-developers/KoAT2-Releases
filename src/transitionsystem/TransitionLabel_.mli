open! OurBase
(** Modul handles labels of classical transitions. *)

(** A transition is an edge of a transition system.
    It connects two locations and is annotated with a guard and an update.
    A guard has to be fulfiled for a state to reach another state via the transition.
    An update assigns variables a new value as a linear combination of the old values. *)

module Make (P : PolyTypes.Polynomial) : sig
  include ProgramTypes.TransitionLabel
  module Invariant = Guard

  val mk :
    id:int option ->
    cost:Polynomials.Polynomial.t ->
    assignments:P.t list ->
    patterns:Var.t list ->
    guard:Guard.t ->
    t

  val mk_map :
    id:int option ->
    cost:Polynomials.Polynomial.t ->
    update:P.t ProgramTypes.VarMap.t ->
    guard:Guard.t ->
    invariant:Invariant.t ->
    t

  val add_invariant : t -> Invariant.t -> t
  (** Adds an invariant, i.e., a constraint that is always fulfilled upon execution of the transition, to the transition label *)

  val update_map : t -> P.t ProgramTypes.VarMap.t
  (** Returns the update map of the transitionlabel *)

  val update : t -> Var.t -> P.t Option.t
  (** Returns the update of a variable. *)

  val fresh_id : t -> t
  (** Assign a fresh id to the transition *)

  val separate_guard_invariant : t -> Invariant.t -> t
  (** We execute CFRefinement with guard && invariant -> We need to separate invariant afterwards. *)

  val only_update : t -> t
  (** Sets costs to 1, and guard and invariant to true *)

  val input_size : t -> int
  (** Returns the number of variables. *)

  val has_tmp_vars : t -> bool

  val update_to_file_string_lhs : t -> string
  (** Returns a string representing the left hand side of the update function.
      Can be used to dump the Program to a file. *)

  val update_to_file_string_rhs : t -> string
  (** Returns a string representing the right hand side of the update function.
      Can be used to dump the Program to a file. *)

  val rename : RenameMap.t -> t -> t
  val tmp_vars_update : t -> VarSet.t

  val rename_temp_vars : t -> Var.t Sequence.t -> t
  (** Rename temporary variables to identifiers provided by the (possibly infinite) sequence *)

  val copy_rename : (int, int) Hashtbl.t -> t -> t
  (** Create an equivalent label with new id's, takes gt_id from the provided table and
        if not available creates and adds a new id_for the general transition to the table.
        *)
end

module TransitionLabelNonRec_ : sig
  include ProgramTypes.ClassicalTransitionLabelNonRec (* TODO Nils can we reuse Make? *)
  module Invariant = Guard

  val mk :
    id:int option ->
    cost:Polynomials.Polynomial.t ->
    assignments:Polynomials.Polynomial.t list ->
    patterns:Var.t list ->
    guard:Guard.t ->
    t

  val chain_guards : t -> t -> Invariant.t
  val append : t -> t -> t
end

include module type of Make (PolyFunctionCall)
include ProgramTypes.ClassicalTransitionLabel with type t := t

val eliminate_tmp_var : Var.t -> t -> t MaybeChanged.t

module TransitionLabelNonRec = TransitionLabelNonRec_
module UpdateElementNonRec = Polynomials.Polynomial

val overapprox_nonlinear_updates : t -> t
val has_rec_calls : t -> bool
val of_non_rec : TransitionLabelNonRec.t -> t
val to_non_rec : t -> TransitionLabelNonRec.t
val overapprox_rec_updates : t -> TransitionLabelNonRec.t
val chain_guards : t -> t -> Guard.t
