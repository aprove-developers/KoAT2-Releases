open Batteries
open ProgramTypes
open BoundsInst

type kind = [ `Lower | `Upper]

val compute: kind -> Program.t -> (kind -> Transition.t -> Var.t -> Bound.t) -> 
             (kind -> GeneralTransition.t * Location.t -> Var.t -> RealBound.t) ->
             (Transition.t -> Bound.t) -> ((GeneralTransition.t * Location.t) * Var.t) -> RealBound.t

