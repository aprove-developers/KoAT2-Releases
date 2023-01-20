open Formulas
open Polynomials
open ProgramTypes

module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  open PM
  type t = Formula.t * (Polynomial.t VarMap.t)

  val mk : TransitionLabel.t -> t

  val guard : t -> Formula.t

  val update : t -> Polynomial.t VarMap.t
  val update_opt : t -> Var.t -> Polynomial.t option
  val update_var : t -> Var.t -> Polynomial.t
  val updated_vars : t -> VarSet.t

  val to_string : t -> string

  val append : t -> t -> t
  val chain : t -> t

  val eliminate_non_contributors : ?relevant_vars:VarSet.t option -> t -> Formula.t * UpdateElement.t VarMap.t

end
