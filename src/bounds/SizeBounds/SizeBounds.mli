(** Performs a single improvement step for a whole program to find better size-bounds. *)

open Batteries

module Make (PM : ProgramTypes.ClassicalProgramModules) : sig
  (** Performs a single improvement step for a whole program to find better size-bounds. *)

  module LSB : module type of LocalSizeBound.Make (PM.TransitionLabel) (PM.Transition) (PM.Program)

  val improve :
    PM.Program.t ->
    RVGTypes.MakeRVG(PM).t * RVGTypes.MakeRVG(PM).scc list Lazy.t ->
    ?scc:PM.TransitionSet.t option ->
    (PM.Transition.t * Var.t -> (LSB.t * bool Lazy.t) Option.t) ->
    Approximation.MakeForClassicalAnalysis(PM).t ->
    Approximation.MakeForClassicalAnalysis(PM).t
  (** Performs a single improvement step for a whole program to find better sizebounds for the approximation and updates the approximation. *)

  val improve_scc :
    PM.Program.t ->
    RVGTypes.MakeRVG(PM).t ->
    (PM.Transition.t * Var.t -> (LSB.t * bool Lazy.t) Option.t) ->
    Approximation.MakeForClassicalAnalysis(PM).t ->
    RVGTypes.MakeRV(PM.TransitionLabel)(PM.Transition).t list ->
    Approximation.MakeForClassicalAnalysis(PM).t
  (** Performs a single improvement step for a single scc to find better sizebounds for the approximation and updates the approximation. *)
end

include module type of Make (ProgramModules)
