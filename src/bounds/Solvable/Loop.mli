open Formulas
open Polynomials

type invariance_cache
(** Results of [check_update_invariant]. Many loops of an scc share atoms and updates, so a cache avoids repeating
    the same SMT queries. It is defined outside of Make, so that all instances of Make share the type. *)

module Make (Bound : BoundType.Bound) (TL : ProgramTypes.ClassicalTransitionLabelNonFunctionCall) : sig
  type t = Formula.t * Formula.t * Polynomial.t ProgramTypes.VarMap.t

  val mk : TL.t -> t
  val guard : t -> Formula.t
  val guard_without_inv : t -> Formula.t
  val update : t -> Polynomial.t ProgramTypes.VarMap.t
  val update_opt : t -> Var.t -> Polynomial.t option
  val update_var : t -> Var.t -> Polynomial.t
  val updated_vars : t -> VarSet.t
  val vars : t -> VarSet.t
  val to_string : t -> string
  val append : t -> t -> t
  val chain : t -> t
  val eliminate_non_contributors : ?relevant_vars:VarSet.t option -> t -> t
  val compute_bound_n_iterations : t -> Var.t -> int -> Bound.t
  val substition_unsolvable : t -> Polynomial.t -> Var.t -> t
  val commuting : t -> t -> bool

  type nonrec invariance_cache = invariance_cache

  val create_invariance_cache : unit -> invariance_cache

  val check_update_invariant : invariance_cache -> t -> Atoms.Atom.t -> bool
  (** [check_update_invariant cache loop atom] is true iff [atom] is invariant under the update of [loop]. *)
end
