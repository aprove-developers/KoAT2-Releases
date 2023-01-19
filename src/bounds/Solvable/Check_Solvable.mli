module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  module Loop: module type of Loop.Make(PM)

  type blocks = Var.t list list

  val check_solvable : Loop.t -> blocks option

  val check_solvable_ : PM.Transition.t -> blocks option
end
