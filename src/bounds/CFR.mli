(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

(** Logger CFR *)
val logger : Batteries.Logger.log

(** Unrolls all transitions listed in nonLinearTransitions.starrt  *)
val apply_cfr : Program.t -> unit

(** Reference is used to store non-linear transitions. *)
val nonLinearTransitions : ProgramTypes.TransitionSet.t Batteries.ref
