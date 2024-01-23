open! OurBase

type config = { abstract : [ `FVS | `LoopHeads ] }

module ClassicPartialEvaluation : sig
  open ProgramModules

  val evaluate_program : config -> Program.t -> Program.t
  val apply_sub_scc_cfr : config -> TransitionSet.t -> Program.t -> Program.t
end

module ProbabilisticPartialEvaluation : sig
  open ProbabilisticProgramModules

  val evaluate_program : config -> Program.t -> Program.t
  val apply_sub_scc_cfr : config -> TransitionSet.t -> Program.t -> Program.t
end
