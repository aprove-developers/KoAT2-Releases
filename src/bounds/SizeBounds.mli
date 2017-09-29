open Batteries

(** Performs a single improvement step to find better sizebounds for the approximation and updates the approximation. *)
val improve : Program.t -> Approximation.t -> Approximation.t
