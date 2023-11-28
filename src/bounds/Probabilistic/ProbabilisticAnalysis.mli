open! OurBase
open ProbabilisticProgramModules

type configuration = { compute_refined_plrfs : bool }

val default_configuration : configuration
(** The default configuration where [ compute_refined_plrfs = false ] *)

val perform_analysis :
  ?classic_conf:(NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration ->
  ?conf:configuration ->
  Program.t ->
  Program.t * Approximation.Probabilistic.apprs
