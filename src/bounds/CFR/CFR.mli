open! OurBase

type 'prog_modules_t cfr_

module Classical (Bound : BoundType.Bound) : sig
  include
    CFRTypes.CFR
      with type program = ProgramModules.Program.t
       and type transition = ProgramModules.Transition.t
       and type transition_set = ProgramModules.TransitionSet.t
       and type cfr = ProgramModules.program_modules_t cfr_

  type appr = Approximation.MakeForClassicalAnalysis(Bound)(ProgramModules).t

  val create_new_appr : program -> program -> appr -> appr
  (** The call [create_new_appr program program_cfr appr] generates the approximation for the new program_cfr from the one of the original program ([appr]).
      Also computes trivial time bounds *)
end

module Probabilistic : sig
  include
    CFRTypes.CFR
      with type program = ProbabilisticProgramModules.Program.t
       and type transition = ProbabilisticProgramModules.Transition.t
       and type transition_set = ProbabilisticProgramModules.TransitionSet.t
       and type cfr = ProbabilisticProgramModules.program_modules_t cfr_

  val create_new_apprs :
    program -> program -> Approximation.Probabilistic.apprs -> Approximation.Probabilistic.apprs
  (** The call [create_new_appr program program_cfr apprs] generates the approximations for the new program_cfr from the those of the original program ([apprs]).
      Also computes trivial time bounds *)
end

val pe_with_IRankFinder : ProgramModules.program_modules_t cfr_
(** PartialEvaluation with IRankFinder *)

val chaining : ProgramModules.program_modules_t cfr_
(** Chaining *)

val pe_native : NativePartialEvaluation.config -> ProgramModules.program_modules_t cfr_

val pe_native_probabilistic :
  NativePartialEvaluation.config -> ProbabilisticProgramModules.program_modules_t cfr_
