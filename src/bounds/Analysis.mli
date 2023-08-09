open Batteries
(** Performs improvement steps for the whole program to find better time-bounds. *)

open Bounds
open ProgramTypes
open RVGTypes

(** These types are used to limit certain analyses methods to the specified underlying types.
    Together with the GADTs they are used to prove type equalities *)
type !'prog_modules_t cfr_configuration =
  | NoCFR : 'a cfr_configuration
  | PerformCFR : [ `Chaining | `PartialEvaluation ] list -> ProgramModules.program_modules_t cfr_configuration

type !'prog_modules_t closed_form_size_bounds =
  | NoClosedFormSizeBounds : 'a closed_form_size_bounds
  | ComputeClosedFormSizeBounds : ProgramModules.program_modules_t closed_form_size_bounds

type !'prog_modules_t analysis_configuration = {
  run_mprf_depth : int option;
  twn_configuration : TWN.configuration option;
  cfr_configuration : 'prog_modules_t cfr_configuration;
  closed_form_size_bounds : 'prog_modules_t closed_form_size_bounds;
  analysis_type : [ `Termination | `Complexity ];
}

type classical_program_conf_type = ProgramModules.program_modules_t analysis_configuration
(** This corresponds to strictly classical programs only.
    So not even overapproximated probabilistic progams are allowed.  *)

val default_configuration : 'a analysis_configuration
(** Default configuration. mprf_depth of 1, no twn, no cfr, and no closed form size bounds*)

module Make (PM : ProgramTypes.ClassicalProgramModules) : sig
  (** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

  type allowed_conf_type = PM.program_modules_t analysis_configuration
  (** The type of the configuration for the program *)

  val improve :
    ?time_cfr:int ->
    conf:allowed_conf_type ->
    preprocess:(PM.Program.t -> PM.Program.t) ->
    PM.Program.t ->
    Approximation.MakeForClassicalAnalysis(PM).t ->
    PM.Program.t * Approximation.MakeForClassicalAnalysis(PM).t
  (** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
end
