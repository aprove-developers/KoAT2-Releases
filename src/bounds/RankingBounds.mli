open Batteries
open ProgramTypes

(**Collect all non-linear bounds 
val nonLinearTransitions : ProgramTypes.TransitionSet.t Batteries.ref*)

(** Performs a single improvement step to find better timebounds for the approximation and updates the approximation. *)
val improve :  ?mrf:bool -> ?cfr:bool -> [ `Cost | `Time ] -> Program.t -> Approximation.t ->  Approximation.t MaybeChanged.t

val entry_transitions : Program.t -> ProgramTypes.Transition.t list -> ProgramTypes.Transition.t  Batteries.List.t
