module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  module Loop: module type of Loop.Make(PM)

  val check_triangular : Loop.t -> Var.t list

  val check_weakly_negativitiy : Loop.t -> bool

  val check_twn : Loop.t -> bool

  val check_twn_ : PM.Transition.t -> bool
end
