(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

(** Logger CFR *)
val logger : Batteries.Logger.log

val time : float Batteries.ref

(** Returns the number of times a transition was unrolled. This ensures termination. *)
val getLevel : ProgramTypes.Transition.t -> int

(** Unrolls all transitions listed in nonLinearTransitions.starrt  *)
val apply_cfr : Program.t -> Program.t

(** Reference is used to store non-linear transitions. *)
val nonLinearTransitions : ProgramTypes.TransitionSet.t Batteries.ref
