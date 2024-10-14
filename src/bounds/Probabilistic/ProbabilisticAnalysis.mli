open! OurBase
open ProbabilisticProgramModules

module Make : sig
  module BP : module type of BoundPair.PAST

  type configuration = {
    classical_local : (NonProbOverappr.program_modules_t, BP.ClassBound.t) Analysis.local_configuration;
    cfrs : CFR.Probabilistic(BP).cfr List.t;
  }

  val perform_analysis : configuration -> Program.t -> Program.t * Approximation.Probabilistic(BP).apprs
end

include module type of Make
(** TODO: Remove this *)

val default_configuration : configuration
(** The default configuration where no CFR, and [classical_local = Analysis.default_local_configuration] *)
