open Batteries
open ProgramTypes
open RVGTypes

module RV : sig include module type of RVG.RV end

(** Performs a single improvement step for a whole program to find better sizebounds for the approximation and updates the approximation. *)
val improve : LocalSizeBound.lsb_cache -> Program.t -> Approximation.t -> Approximation.t

(** Performs a single improvement step for a single scc to find better sizebounds for the approximation and updates the approximation. *)
val improve_scc : LocalSizeBound.lsb_cache -> [`Lower | `Upper] -> Program.t -> RVG.t -> Approximation.t -> RV.t list -> Approximation.t
