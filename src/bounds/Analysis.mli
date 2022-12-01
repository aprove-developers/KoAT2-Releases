(** Performs improvement steps for the whole program to find better time-bounds. *)
open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open ProgramModules

type ('a,'b) twn_conf = ('a,'b) TWN.twn_transformation_fun_type
type 'twn_conf local_timebounds_configuration = {
  run_mprf_depth: int option; (** Run mprfs and with which maximum depth? *)
  twn_configuration: 'twn_conf TWN.configuration option
}

(** Performs improvement steps for the whole program to find better time-bounds and triggers control flow refinement if needed. *)

type rvg_with_sccs = RVG.t * RVG.scc list Lazy.t

(** Performs improvement steps to find better timebounds for the approximation and updates the approximation. *)
val improve : rvg_with_sccs -> preprocess:(Program.t -> Program.t)
            -> local:((TWNLoop.Make(ProgramModules).t, Program.transition) twn_conf local_timebounds_configuration)
            -> cfr:([`PartialEvaluation | `Chaining ] list)
            -> [ `Cost | `Time ]
            -> Program.t -> Approximation.t ->  Program.t * Approximation.t
