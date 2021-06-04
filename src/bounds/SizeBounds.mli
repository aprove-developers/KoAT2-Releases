(** Performs a single improvement step for a whole program to find better size-bounds. *)

open Batteries
open ProgramTypes
open RVGTypes
(** Performs a single improvement step for a whole program to find better size-bounds. *)

(** Performs a single improvement step for a whole program to find better sizebounds for the approximation and updates the approximation. *)
val improve : Program.t -> RVG.t -> ?scc:ProgramTypes.TransitionSet.t option -> bool -> Approximation.t -> Approximation.t

(** Performs a single improvement step for a single scc to find better sizebounds for the approximation and updates the approximation. *)
val improve_scc : Program.t -> RVG.t -> Approximation.t -> RV.t list -> Approximation.t
