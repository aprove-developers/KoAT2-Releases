open Batteries
open RVGTypes
open ProgramTypes
open BoundsInst

module RV : module type of Make_RV (RVTransitions.TransitionForExpectedSize)

val compute: CacheManager.t -> Program.t -> (GeneralTransition.t -> Bound.t) -> (GeneralTransition.t -> RealBound.t) ->
             (Transition.t -> Var.t -> Bound.t) -> (GeneralTransition.t * Location.t -> Var.t -> RealBound.t) ->
             RV.t list -> RealBound.t
