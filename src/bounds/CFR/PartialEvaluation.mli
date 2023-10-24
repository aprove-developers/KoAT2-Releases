val uid : string ref
(** Provides control flow refinement on minimal SCCs containing non-linear transitions. *)

module Make (Bound : BoundType.Bound) : sig
  open ProgramModules
  module Approximation := Approximation.MakeForClassicalAnalysis(Bound)(ProgramModules)

  val logger : Batteries.Logger.log
  (** Logger CFR *)

  val add_to_proof : Program.t -> string -> unit

  val time_cfr : float ref
  (** Global time used for CFR. *)

  val compute_timeout_time : Program.t -> Approximation.t -> TransitionSet.t -> float

  val apply_cfr : TransitionSet.t -> Program.t -> Program.t MaybeChanged.t
  (** Unrolls all transitions listed in nonLinearTransitions.starrt  *)

  val applyIrankFinder : Program.t -> Program.t
end
