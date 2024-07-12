module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  module Loop : module type of Loop.Make (Bound) (PM)
  open PolyExponential

  type blocks = Var.t list list

  val check_solvable : Loop.t -> blocks option
  val check_solvable_ : PM.Transition.t -> blocks option
  val compute_closed_form_ : Loop.t -> Var.t list list -> (Var.t * ComplexPE.t) list
  val compute_closed_form : Loop.t -> (Var.t * ComplexPE.t) list option
end
