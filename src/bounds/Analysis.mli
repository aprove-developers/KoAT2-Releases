(** Performs improvement steps for the whole program to find better time-bounds. *)
open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes

(** These types are used to limit certain analyses methods to the specified underlying types. *)
(** Together with the GADTs they are used to prove type equalities *)
type ('prog,'tset,'rvg,'rvg_scc,'twn,'appr) cfr_configuration =
  | NoCFR: ('a,'b,'c,'d,'e,'f) cfr_configuration
  | PerformCFR: [ `Chaining | `PartialEvaluation ] list
              -> ( ProgramModules.Program.t
                , ProgramModules.TransitionSet.t
                , RVGTypes.MakeRVG(ProgramModules).t, RVGTypes.MakeRVG(ProgramModules).scc
                , Loop.Make(ProgramModules).t
                , Approximation.MakeForClassicalAnalysis(ProgramModules).t) cfr_configuration

type ('prog, 'tset, 'appr) closed_form_size_bounds =
  | NoClosedFormSizeBounds: ('prog,'trans_set,'appr) closed_form_size_bounds
  | ComputeClosedFormSizeBounds: (ProgramModules.Program.t,ProgramModules.TransitionSet.t,Approximation.MakeForClassicalAnalysis(ProgramModules).t) closed_form_size_bounds


type ('trans,'prog,'tset,'rvg,'rvg_scc,'twn,'appr) analysis_configuration =
  { run_mprf_depth: int option
  ; twn_configuration: TWN.configuration option
  ; cfr_configuration: ('prog,'tset,'rvg,'rvg_scc,'twn,'appr) cfr_configuration
  ; closed_form_size_bounds: ('prog, 'tset,'appr) closed_form_size_bounds
  }

type classical_program_conf_type = ( ProgramModules.Transition.t
                                   , ProgramModules.Program.t
                                   , ProgramModules.TransitionSet.t
                                   , RVGTypes.MakeRVG(ProgramModules).t
                                   , RVGTypes.MakeRVG(ProgramModules).scc
                                   , Loop.Make(ProgramModules).t
                                   , Approximation.MakeForClassicalAnalysis(ProgramModules).t ) analysis_configuration


(** Default configuration. mprf_depth of 1, no twn, no cfr, and no closed form size bounds*)
val default_configuration: ('a,'b,'c,'d,'e,'f,'g) analysis_configuration

val termination_only: bool -> unit

val only_relax_loops: bool -> unit

module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  (** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

  (** The type of the configuration for the program *)
  type conf_type =
    (PM.Transition.t,PM.Program.t,PM.TransitionSet.t,
     MakeRVG(PM).t,MakeRVG(PM).scc,Loop.Make(PM).t, Approximation.MakeForClassicalAnalysis(PM).t) analysis_configuration

  (** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
  val improve : conf:conf_type
              -> preprocess:(PM.Program.t -> PM.Program.t)
              -> PM.Program.t -> Approximation.MakeForClassicalAnalysis(PM).t
              -> PM.Program.t * Approximation.MakeForClassicalAnalysis(PM).t
end
