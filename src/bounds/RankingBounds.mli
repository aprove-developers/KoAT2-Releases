(** Performs improvement steps for the whole program to find better time-bounds. *)
open Batteries
open BoundsInst
open ProgramTypes

(** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

(** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
val improve : RVGTypes.RVG.t -> ?mprf_max_depth:int -> ?cfr:bool -> ?inv:bool -> ?fast:bool -> [ `Cost | `Time ] -> Program.t -> Approximation.t ->  Program.t * Approximation.t
