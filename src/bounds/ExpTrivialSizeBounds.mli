open Batteries
open ProgramTypes
open BoundsInst

type kind = [ `Lower | `Upper]

val compute: ExpLocalSizeBound.elsb_cache -> Program.t -> (Transition.t * Var.t -> Bound.t) ->
             (GeneralTransition.t * Location.t -> Var.t -> RealBound.t) ->
             (GeneralTransition.t -> Bound.t) -> ((GeneralTransition.t * Location.t) * Var.t) -> RealBound.t