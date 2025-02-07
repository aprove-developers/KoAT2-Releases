module Make (Bound : BoundType.Bound) (TL : ProgramTypes.ClassicalTransitionLabelNonRec) : sig
  module Loop : module type of Loop.Make (Bound) (TL)
  open PolyExponential

  type blocks = Var.t list list

  val check_solvable : Loop.t -> blocks option
  val check_solvable_ : TL.t -> blocks option
  val compute_closed_form : Loop.t -> (Var.t * ComplexPE.t) list option
end
