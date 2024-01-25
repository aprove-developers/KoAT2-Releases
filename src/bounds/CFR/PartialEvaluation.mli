open ProgramModules

val uid : string ref
(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

val logger : Batteries.Logger.log
(** Logger CFR *)

val add_to_proof : Program.t -> string -> unit

val apply_cfr : TransitionSet.t -> Program.t -> Program.t MaybeChanged.t
(** Unrolls all transitions listed in nonLinearTransitions.starrt  *)

val applyIrankFinder : Program.t -> Program.t
