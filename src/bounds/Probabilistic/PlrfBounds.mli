open! OurBase
open ProbabilisticProgramModules

module Make (BP : BoundPair.T) : sig
  open Approximation.Probabilistic(BP)

  val improve_timebounds_plrf :
    Program.t ->
    GeneralTransitionSet.t ->
    ClassicalApproximation.t * ExpApproximation.t ->
    ExpApproximation.t MaybeChanged.t
  (** Improve the approximation by computing new PLRFs and lifting them to new expected time bound *)
end
