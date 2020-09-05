open Batteries
open ProgramTypes
open RVGTypes

module RV : sig include module type of RVG.RV end

(** Performs a single improvement step for a whole program to find better sizebounds for the approximation and updates the approximation. *)
val improve : CacheManager.t -> Program.t -> Approximation.t -> Approximation.t

