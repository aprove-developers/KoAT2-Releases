open Batteries
open ProbabilisticProgramModules

val perform_analysis: Program.t
                      -> Approximation.Probabilistic.ClassicApproximation.t
                      -> Approximation.Probabilistic.ExpApproximation.t
