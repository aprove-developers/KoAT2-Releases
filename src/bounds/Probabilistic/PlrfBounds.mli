open! OurBase
open ProbabilisticProgramModules
open Approximation.Probabilistic

val improve_timebounds_plrf :
  compute_refined_plrfs:bool ->
  Program.t ->
  GeneralTransitionSet.t ->
  ClassicalApproximation.t * ExpApproximation.t ->
  ExpApproximation.t MaybeChanged.t
(** Improve the approximation by computing new PLRFs and lifting them to new expected time bound *)
