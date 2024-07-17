module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  module Loop : module type of Loop.Make (Bound) (PM)

  type twn = TWN of Var.t list | NOTTRIANGULAR
  type monotonicity = MONOTONIC | DEFECTIVE of Var.t list

  exception NOT_TWN

  val unwrap_twn : twn -> Var.t list
  val check_triangular : Loop.t -> twn
  val check_weakly_negativitiy : Loop.t -> bool
  val check_weakly_monotonicity : Loop.t -> monotonicity
  val defective_vars : monotonicity -> Var.t list
  val check_twn : Loop.t -> bool
end
