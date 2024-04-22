open! OurBase
open ProbabilisticProgramModules

type configuration = {
  classical_local : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.local_configuration;
  cfrs : CFR.Probabilistic.cfr List.t;
}

val default_configuration : configuration
(** The default configuration where no CFR, and [classical_local = Analysis.default_local_configuration] *)

val perform_analysis : ?conf:configuration -> Program.t -> Program.t * Approximation.Probabilistic.apprs
