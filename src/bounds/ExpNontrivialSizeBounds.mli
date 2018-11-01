open Batteries
open RVGTypes
open ProgramTypes
open BoundsInst

module RV : module type of Make_RV (RVTransitions.TransitionForExpectedSize)

type kind = [ `Lower | `Upper ] [@@deriving show]

val compute: kind -> Program.t -> ERVG.t -> (Transition.t -> Bound.t) -> (GeneralTransition.t -> RealBound.t) -> 
             (kind -> Transition.t -> Var.t -> Bound.t) -> (kind -> GeneralTransition.t * Location.t -> Var.t -> RealBound.t) ->
             RV.t list -> RealBound.t
             
