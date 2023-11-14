open! OurBase

(** CFR meta-module *)
module CFR (Bound : BoundType.Bound) : sig
  open ProgramModules

  type approximation = Approximation.MakeForClassicalAnalysis(Bound)(ProgramModules).t

  val time_cfr : float ref
  (** Global time used for CFR. *)

  val compute_timeout_time : Program.t -> approximation -> TransitionSet.t -> float

  val merge_appr : Program.t -> Program.t -> approximation -> approximation
  (** The call [merge_appr program program_cfr appr] generates the approximation for the new program_cfr from the one of the original program ([appr]). *)
end
