(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

(** Logger CFR *)
val logger : Batteries.Logger.log

val delta_current_cfr : float Batteries.ref

val time_current_cfr : float Batteries.ref

val time_cfr : float Batteries.ref

val set_time_current_cfr : ProgramTypes.TransitionSet.t -> Approximation.t -> unit

val poll_timeout : ?applied_cfr:bool -> unit


val number_unsolved_trans : int Batteries.ref

exception TIMEOUT

val uid : string Batteries.ref

(** Unrolls all transitions listed in nonLinearTransitions.starrt  *)
val apply_cfr :  ProgramTypes.TransitionSet.t -> ProgramTypes.IDSet.t -> Program.t -> Approximation.t -> (Program.t * Approximation.t * ProgramTypes.IDSet.t) option