type 'a refinement_result =
  | DontKeepRefinedProgram
  | KeepRefinedProgram of 'a ProofOutput.LocalProofOutput.with_proof

module type CFR = sig
  type program
  type transition
  type transition_set
  type cfr

  val compute_timeout_time : program -> infinite_timebound:(transition -> bool) -> transition_set -> float

  val time_cfr : float ref
  (** Global remaining time for cfr *)

  val mk_cfr :
    method_name:String.t -> (program -> transitions_to_refine:transition_set -> program MaybeChanged.t) -> cfr

  val method_name : cfr -> String.t
  val perform_cfr : cfr -> program -> transitions_to_refine:transition_set -> program MaybeChanged.t

  val iter_cfrs :
    program ->
    scc_orig:transition_set ->
    transitions_to_refine:transition_set ->
    compute_timelimit:(unit -> float) ->
    (cfr -> refined_program:program -> 'a refinement_result) ->
    cfr list ->
    'a ProofOutput.LocalProofOutput.with_proof option
  (** Try to apply multiple CFRs until we decide to keep the refined program *)

  val add_proof_to_global_proof : cfr -> refined_program:program -> refined_bound_str:String.t -> unit
  (** Adds an explanation of the CFR to the global proof *)
end
