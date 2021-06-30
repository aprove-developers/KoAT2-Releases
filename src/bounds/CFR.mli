open ProgramTypes
(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

(** Logger CFR *)
val logger : Batteries.Logger.log

val time_cfr : float Batteries.ref

val compute_timeout_time : Program.t -> Approximation.t -> TransitionSet.t -> float

val uid : string Batteries.ref

(** Unrolls all transitions listed in nonLinearTransitions.starrt  *)
val apply_cfr :  ProgramTypes.TransitionSet.t -> TransitionSet.t -> Program.t -> Approximation.t -> (Program.t * Approximation.t * TransitionSet.t) option
