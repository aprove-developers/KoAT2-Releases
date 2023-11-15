open! OurBase
(** Performs improvement steps for the whole program to find better time-bounds. *)

(** These types are used to limit certain analyses methods to the specified underlying types.
    Together with the GADTs they are used to prove type equalities *)

type !'bound goal = Complexity : Bounds.Bound.t goal | Termination : Bounds.BinaryBound.t goal

type (!'prog_modules_t, 'bound) closed_form_size_bounds =
  | NoClosedFormSizeBounds : ('a, 'b) closed_form_size_bounds
  | ComputeClosedFormSizeBounds : (ProgramModules.program_modules_t, Bounds.Bound.t) closed_form_size_bounds

type (!'prog_modules_t, 'bound) analysis_configuration = {
  run_mprf_depth : int option;
  twn : bool;
  cfrs : 'prog_modules_t CFR.cfr_ list;
  goal : 'bound goal;
  closed_form_size_bounds : ('prog_modules_t, 'bound) closed_form_size_bounds;
}

type classical_program_conf_type = (ProgramModules.program_modules_t, Bounds.Bound.t) analysis_configuration
(** This corresponds to strictly classical programs only.
    So not even overapproximated probabilistic progams are allowed.  *)

val default_configuration : ('a, Bounds.Bound.t) analysis_configuration
(** Default configuration. mprf_depth of 1, no twn, no cfr, and no closed form size bounds*)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  (** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

  type allowed_conf_type = (PM.program_modules_t, Bound.t) analysis_configuration
  (** The type of the configuration for the program *)

  val improve :
    ?time_cfr:int ->
    conf:allowed_conf_type ->
    preprocess:(PM.Program.t -> PM.Program.t) ->
    PM.Program.t ->
    Approximation.MakeForClassicalAnalysis(Bound)(PM).t ->
    PM.Program.t * Approximation.MakeForClassicalAnalysis(Bound)(PM).t
  (** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
end
