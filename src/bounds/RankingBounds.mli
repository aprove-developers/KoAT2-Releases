(** Updates all time-bounds and triggers control flow refinement if needed. *)
open Batteries
open ProgramTypes


(** Performs a single improvement step to find better timebounds for the approximation and updates the approximation. *)
val improve :  ?mrf:bool -> ?cfr:bool -> [ `Cost | `Time ] -> Program.t -> Approximation.t ->  Approximation.t MaybeChanged.t

(** Computes all entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. TODO Move to Program module? *)
val entry_transitions : Program.t -> ProgramTypes.Transition.t list -> ProgramTypes.Transition.t  Batteries.List.t
