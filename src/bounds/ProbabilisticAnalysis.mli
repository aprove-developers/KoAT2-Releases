open Batteries
open ProbabilisticProgramModules
open Approximation.Probabilistic

type configuration = { compute_refined_plrfs: bool }

(** The default configuration where [ compute_refined_plrfs = false ] *)
val default_configuration: configuration

val perform_analysis: ?conf:configuration
                      -> Program.t
                      -> ClassicalApproximation.t
                      -> ExpApproximation.t

val perform_classic_and_probabilistic_analysis: ?classic_conf:Analysis.Make(NonProbOverappr).conf_type
                                                -> ?conf:configuration
                                                -> preprocess:(Program.t -> Program.t)
                                                -> Program.t
                                                -> Program.t * (ClassicalApproximation.t * ExpApproximation.t)
