open Automorphism
open Atoms
open Batteries
open BoundsInst
open Constraints
open Formulas
open PolyExponential
open Polynomials
open ProgramModules

let logger = Logging.(get Twn)

module SMTSolver = SMT.Z3Solver
module SMTSolverTimeout = SMT.Z3SolverTimeout

type ('twnt,'tt) twn_transformation_fun_type =
  'tt * ('tt list * 'tt list) * 'twnt -> ('tt * ('tt list * 'tt list) * 'twnt * Automorphism.t) option

type twn_transformation_fun_type_transformable =
  (TWNLoop.Make(ProgramModules).t, ProgramModules.Transition.t) twn_transformation_fun_type

type _ configuration = NoTransformation: ('a,'b) twn_transformation_fun_type configuration
                     | Transformation: Transformation.transformation_type
                                     -> (TWNLoop.Make(ProgramModules).t,
                                         ProgramModules.Transition.t) twn_transformation_fun_type configuration

(** Applies any required transformation *)
let handle_transformation (type a) (conf: a configuration): a =
  match conf with | Transformation transformation -> Transformation.transform transformation
                  | NoTransformation -> fun (a,b,c) -> Some (a,b,c,Automorphism.identity_aut)

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Approximation = Approximation.MakeForClassicalAnalysis(PM)
  module InvariantGeneration = InvariantGeneration.Make(PM)
  module TWNLoop = TWNLoop.Make(PM)
  module TWN_Complexity = TWN_Complexity.Make(PM)
  module TWN_Termination = TWN_Termination.Make(PM)
  module Check_TWN = Check_TWN.Make(PM)
  module SimpleCycle = SimpleCycle.SimpleCycle(PM)

  module TimeBoundTable = Hashtbl.Make(Transition)

  (* Keys: transition, values: bounds of entry transitions. *)
  let time_bound_table: (Transition.t * Bound.t) list TimeBoundTable.t = TimeBoundTable.create 10

  let lift appr entry bound =
    let bound_with_sizebound = Bound.substitute_f (Approximation.sizebound appr entry) bound in
      Bound.mul (Approximation.timebound appr entry) bound_with_sizebound
      |> tap @@ fun b ->
      TWN_Proofs.proof_append @@ FormattedString.(mk_paragraph (mk_str_line ("relevant size-bounds w.r.t. t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ":") <> (
            Bound.vars bound
            |> VarSet.to_list
            |> List.map (fun v -> (Var.to_string ~pretty:true v) ^ ": " ^ (Approximation.sizebound appr entry v |> Bound.to_string ~pretty:true))
            |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend) <>
            FormattedString.mk_str_line ("Runtime-bound of t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ": " ^ (Approximation.timebound appr entry |> Bound.to_string ~pretty:true)) <>
            FormattedString.mk_str ("Results in: " ^ (Bound.to_string ~pretty:true b))))

  let heuristic_for_cycle appr entry program loop =
    Check_TWN.check_twn_loop loop && Approximation.is_time_bounded appr entry

  let time_bound (l,t,l') scc program appr transformation_type =
    TWN_Proofs.proof := FormattedString.Empty;
    let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in
    if Option.is_none opt then (
      let bound = Timeout.timed_run 5. (fun () ->
      (* We have not yet computed a (local) runtime bound. *)
      let loops_opt = SimpleCycle.find_loops heuristic_for_cycle appr program scc t in
      if Option.is_some loops_opt then
        let cycle, loops = Option.get loops_opt in
        let local_bounds = List.map (fun (entry,loop) -> entry, TWN_Complexity.complexity loop) loops in
        List.iter (fun t -> TimeBoundTable.add time_bound_table t local_bounds) cycle;
        List.map (Tuple2.uncurry @@ lift appr) local_bounds
        |> List.enum
        |> Bound.sum
      else (
        TimeBoundTable.add time_bound_table (l,t,l') [(l,t,l'),Bound.infinity];
        Bound.infinity)) in
      if Option.is_some bound then
        Tuple2.first @@ Option.get bound
      else
        Bound.infinity
    ) else (
      (* We already have computed a (local) runtime bound and just lift it again.*)
      let xs = Option.get opt in
      let bound_with_sizebound = Bound.sum_list (List.map (Tuple2.uncurry @@ lift appr) xs) in
      bound_with_sizebound
    )
end
