open! OurBase
open ProbabilisticProgramModules
open Approximation.Probabilistic

type configuration = { compute_refined_plrfs : bool }

val default_configuration : configuration
(** The default configuration where [ compute_refined_plrfs = false ] *)

val perform_analysis :
  ?classic_conf:(NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration ->
  ?conf:configuration ->
  Program.t ->
  ClassicalApproximation.t ->
  ExpApproximation.t

val perform_classic_and_probabilistic_analysis :
  ?classic_conf:(NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration ->
  ?conf:configuration ->
  Program.t ->
  Program.t * (ClassicalApproximation.t * ExpApproximation.t)
