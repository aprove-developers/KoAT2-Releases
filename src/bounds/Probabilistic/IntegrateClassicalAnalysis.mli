open OurBase
open ProbabilisticProgramModules
open Approximation.Probabilistic

val improve :
  mprf_depth:int Option.t ->
  Program.t * GeneralTransitionSet.t ->
  GeneralTransitionSet.t ->
  ClassicalApproximation.t ->
  ExpApproximation.t ->
  ExpApproximation.t MaybeChanged.t
