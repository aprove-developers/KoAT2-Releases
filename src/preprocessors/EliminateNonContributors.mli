(** Implementation of a preprocessor which eliminates variables that do not contribute to guards. *)

open! OurBase
open Polynomials

(** Handles function calls, which differ between classical and probabilistic programs. *)
module type Adapter = sig
  type transition
  type transition_label

  val vars_with_function_call_calls : transition -> VarSet.t
  (** Returns the variables whose update contains a function call. They cannot be removed. *)

  val dependency_rec : transition_label -> Var.t -> Var.t -> bool
  (** [dependency_rec t x y] is true iff x depends on y in a recursive call of t.
      The partial application [dependency_rec t] may perform expensive precomputations on [t]. *)
end

(** Adapter for classical programs, which may contain function calls. *)
module ClassicAdapter (M : ProgramTypes.ClassicalProgramModules) :
  Adapter with type transition = M.Transition.t and type transition_label = M.TransitionLabel.t

(** Adapter for programs without function calls. *)
module DefaultAdapter : sig
  val vars_with_function_call_calls : 'transition -> VarSet.t
  val dependency_rec : 'transition_label -> Var.t -> Var.t -> bool
end

module Make
    (M : ProgramTypes.ProgramModules)
    (_ : Adapter with type transition := M.Transition.t and type transition_label := M.TransitionLabel.t) : sig
  val eliminate : Logger.log -> M.Program.t -> M.Program.t MaybeChanged.t
  (** Removes all variables from the program that do not contribute to a guard or a cost. *)
end

val eliminate_t :
  Logger.log -> VarSet.t -> VarSet.t -> (Var.t -> Polynomial.t option) -> (VarSet.t -> 'a) -> 'a
(** [eliminate_t logger vars vars_guard get_update remove_non_contributors] computes the variables of [vars] that
    do not contribute to [vars_guard] w.r.t. the updates [get_update], and passes them to
    [remove_non_contributors]. *)
