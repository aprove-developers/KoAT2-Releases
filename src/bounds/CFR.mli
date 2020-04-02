(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

(** Logger CFR *)
val logger : Batteries.Logger.log

val delta_current_cfr : float Batteries.ref

exception TIMEOUT

val random : int Batteries.ref

(** Returns the number of times a transition was unrolled. This ensures termination. *)
val already_used_cfr : ProgramTypes.TransitionSet.t Batteries.ref

(** Unrolls all transitions listed in nonLinearTransitions.starrt  *)
val apply_cfr : Program.t -> Approximation.t -> (Program.t * Approximation.t)

(** Reference is used to store non-linear transitions. *)
val nonLinearTransitions : ProgramTypes.TransitionSet.t Batteries.ref
