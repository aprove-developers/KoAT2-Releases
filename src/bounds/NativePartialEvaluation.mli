open! OurBase

type config = { abstract : [ `FVS | `LoopHeads ]; k_encounters : int; update_invariants : bool }

module ClassicPartialEvaluation : sig
  open ProgramModules

  val evaluate_program : config -> Program.t -> Program.t
  val evaluate_transitions : config -> Program.t -> TransitionSet.t -> Program.t
  val apply_sub_scc_cfr : config -> TransitionSet.t -> Program.t -> Program.t
end

module ProbabilisticPartialEvaluation : sig
  open ProbabilisticProgramModules

  val evaluate_program : config -> Program.t -> Program.t
  val evaluate_transitions : config -> Program.t -> TransitionSet.t -> Program.t
  val apply_sub_scc_cfr : config -> TransitionSet.t -> Program.t -> Program.t
end
