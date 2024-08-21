open! OurBase
(** Performs improvement steps for the whole program to find better time-bounds. *)

(** These types are used to limit certain analyses methods to the specified underlying types.
    Together with the GADTs they are used to prove type equalities *)

type !'bound goal = Complexity : Bounds.Bound.t goal | Termination : Bounds.BinaryBound.t goal

type (!'prog_modules_t, 'bound) closed_form_size_bounds =
  | NoClosedFormSizeBounds : ('a, 'b) closed_form_size_bounds
  | ComputeClosedFormSizeBounds : ('a, Bounds.Bound.t) closed_form_size_bounds

type (!'prog_modules_t, !'bound) local_configuration = {
  run_mprf_depth : int option;
  twn : bool;
  twnlog : bool;
  unsolvable : bool;
  commuting : bool;
  closed_form_size_bounds : ('prog_modules_t, 'bound) closed_form_size_bounds;
  goal : 'bound goal;
}

type (!'prog_modules_t, !'bound) analysis_configuration = {
  local_configuration : ('prog_modules_t, 'bound) local_configuration;
  cfrs : 'prog_modules_t CFR.cfr_ list;
}

type classical_program_conf_type = (ProgramModules.program_modules_t, Bounds.Bound.t) analysis_configuration
(** This corresponds to strictly classical programs only.
    So not even overapproximated probabilistic progams are allowed.  *)

val default_local_configuration : ('a, Bounds.Bound.t) local_configuration
(** Default local configuration. mprf_depth of 1, no twn, and no closed form size bounds*)

val default_configuration : ('a, Bounds.Bound.t) analysis_configuration
(** Default configuration. mprf[default_local_configuration] + no cfr *)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  (** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

  type allowed_local_conf_type = (PM.program_modules_t, Bound.t) local_configuration
  (** The type of the configuration for the program *)

  type appr = Approximation.MakeForClassicalAnalysis(Bound)(PM).t

  val improve_scc : conf:allowed_local_conf_type -> LocationSet.t -> PM.Program.t -> appr -> appr
end

module Classical (Bound : BoundType.Bound) : sig
  open ProgramModules

  type appr = Approximation.MakeForClassicalAnalysis(Bound)(ProgramModules).t
  type allowed_conf_type = (program_modules_t, Bound.t) analysis_configuration

  val improve :
    ?time_cfr:int ->
    conf:allowed_conf_type ->
    preprocess:(Program.t -> Program.t) ->
    Program.t ->
    appr ->
    Program.t * appr
  (** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
end
