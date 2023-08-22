open ProgramModules
(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

val logger : Batteries.Logger.log
(** Logger CFR *)

val add_to_proof : Program.t -> string -> unit
val time_cfr : float ref
val compute_timeout_time : Program.t -> Approximation.t -> TransitionSet.t -> float
val uid : string ref

val apply_cfr : TransitionSet.t -> Program.t -> Program.t MaybeChanged.t
(** Unrolls all transitions listed in nonLinearTransitions.starrt  *)

val applyIrankFinder : Program.t -> Program.t
