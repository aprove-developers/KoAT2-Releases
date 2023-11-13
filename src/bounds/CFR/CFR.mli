open! OurBase

(** CFR meta-module *)
module CFR (Bound : BoundType.Bound) : sig
  open ProgramModules

  type approximation = Approximation.MakeForClassicalAnalysis(Bound)(ProgramModules).t

  val merge_appr : Program.t -> Program.t -> approximation -> approximation
  (** The call [merge_appr program program_cfr appr] generates the approximation for the new program_cfr from the one of the original program ([appr]). *)

  val lift_to_program :
    (TransitionGraph.t -> TransitionGraph.t MaybeChanged.t) -> Program.t -> Program.t MaybeChanged.t
end
