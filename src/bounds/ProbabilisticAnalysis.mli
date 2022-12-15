open Batteries
open ProbabilisticProgramModules

type configuration = { compute_refined_plrfs: bool }

val perform_analysis: ?conf:configuration
                      -> Program.t
                      -> Approximation.Probabilistic.ClassicalApproximation.t
                      -> Approximation.Probabilistic.ExpApproximation.t
