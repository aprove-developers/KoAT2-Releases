open! OurBase

type 'prog_modules_t cfr_

(** CFR meta-module *)
module CFR (PM : ProgramTypes.ProgramModules) (Bound : BoundType.Bound) : sig
  open PM

  type approximation = Approximation.MakeForClassicalAnalysis(Bound)(PM).t
  type cfr = PM.program_modules_t cfr_

  val time_cfr : float ref
  (** Global time used for CFR. *)

  val compute_timeout_time : Program.t -> approximation -> TransitionSet.t -> float

  val mk_cfr :
    method_name:String.t ->
    (Program.t -> critical_transitions:TransitionSet.t -> Program.t MaybeChanged.t) ->
    cfr

  val method_name : cfr -> String.t
  val perform_cfr : cfr -> Program.t -> critical_transitions:TransitionSet.t -> Program.t MaybeChanged.t

  val merge_appr : Program.t -> Program.t -> approximation -> approximation
  (** The call [merge_appr program program_cfr appr] generates the approximation for the new program_cfr from the one of the original program ([appr]). *)

  val add_proof_to_global_proof : cfr -> refined_program:Program.t -> refined_bound_str:String.t -> unit
  (** Adds an explanation of the CFR to the global proof *)
end

val pe_with_IRankFinder : ProgramModules.program_modules_t cfr_
(** PartialEvaluation with IRankFinder *)

val chaining : ProgramModules.program_modules_t cfr_
(** Chaining *)

val pe_native : NativePartialEvaluation.config -> ProgramModules.program_modules_t cfr_

val pe_native_probabilistic :
  NativePartialEvaluation.config -> ProbabilisticProgramModules.program_modules_t cfr_
