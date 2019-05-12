open Batteries
open RVGTypes
open ProgramTypes
open BoundsInst

module RV : module type of Make_RV (RVTransitions.TransitionForExpectedSize)

val compute: Program.t -> ERVG.t -> (Transition.t -> Bound.t) -> (GeneralTransition.t -> RealBound.t) ->
             (Transition.t -> Var.t -> Bound.t) -> (GeneralTransition.t * Location.t -> Var.t -> RealBound.t) ->
             RV.t list -> Var.t -> RealBound.t

