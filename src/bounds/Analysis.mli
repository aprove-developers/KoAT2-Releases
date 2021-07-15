(** Performs improvement steps for the whole program to find better time-bounds. *)
open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes

(** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

type rvg_with_sccs = RVG.t * RVG.scc list Lazy.t

(** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
val improve : rvg_with_sccs -> ?mprf_max_depth:int -> ?cfr:bool -> ?inv:bool -> ?fast:bool -> ?twn:bool -> [ `Cost | `Time ] -> Program.t -> Approximation.t ->  Program.t * Approximation.t
