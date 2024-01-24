open! OurBase

module ClassicPartialEvaluation : sig
  open ProgramModules

  val evaluate_program : Abstraction.config -> Program.t -> Program.t
  val apply_sub_scc_cfr : Abstraction.config -> TransitionSet.t -> Program.t -> Program.t
end

module ProbabilisticPartialEvaluation : sig
  open ProbabilisticProgramModules

  val evaluate_program : Abstraction.config -> Program.t -> Program.t
  val apply_sub_scc_cfr : Abstraction.config -> TransitionSet.t -> Program.t -> Program.t
end
