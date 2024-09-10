open Formulas
open Polynomials

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  open PM

  type t = Formula.t * Polynomial.t ProgramTypes.VarMap.t

  val mk : TransitionLabel.t -> t
  val guard : t -> Formula.t
  val update : t -> Polynomial.t ProgramTypes.VarMap.t
  val update_opt : t -> Var.t -> Polynomial.t option
  val update_var : t -> Var.t -> Polynomial.t
  val updated_vars : t -> VarSet.t
  val vars : t -> VarSet.t
  val to_string : t -> string
  val append : t -> t -> t
  val chain : t -> t

  val eliminate_non_contributors :
    ?relevant_vars:VarSet.t option -> t -> Formula.t * UpdateElement.t ProgramTypes.VarMap.t

  val compute_bound_n_iterations : t -> Var.t -> int -> Bound.t
  val substition_unsolvable : t -> Polynomial.t -> Var.t -> t
  val commuting : t -> t -> bool
  val check_update_invariant : t -> Atoms.Atom.t -> bool
end
