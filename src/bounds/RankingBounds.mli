(** Performs improvement steps for the whole program to find better time-bounds. *)
open Batteries
open BoundsInst
open ProgramTypes

(** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

(** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
val improve :  ?mprf:bool -> ?cfr:bool -> ?inv:bool -> [ `Cost | `Time ] -> Program.t -> Approximation.t ->  Program.t * Approximation.t
