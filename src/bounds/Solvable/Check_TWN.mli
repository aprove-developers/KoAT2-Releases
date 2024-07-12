module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  module Loop : module type of Loop.Make (Bound) (PM)

  type t = TWN of Var.t list | NOTTWN of Var.t list

  val unwrap_twn : t -> Var.t list
  val check_triangular : Loop.t -> t
  val check_weakly_negativitiy : Loop.t -> bool
  val check_twn : Loop.t -> bool
  val check_twn_t : PM.Transition.t -> bool
end
