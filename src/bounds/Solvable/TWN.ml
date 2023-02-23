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

type configuration = NoTransformation | Transformation

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Approximation = Approximation.MakeForClassicalAnalysis(PM)
  module Check_TWN = Check_TWN.Make(PM)
  module TWN_Complexity = TWN_Complexity.Make(PM)
  module SimpleCycle = SimpleCycle.Make(PM)
  module Check_Solvable = Check_Solvable.Make(PM)
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

  let heuristic_for_cycle transformation_type appr entry program loop = match transformation_type with
    | NoTransformation -> Check_TWN.check_twn loop && Approximation.is_time_bounded appr entry
    | Transformation -> Option.is_some @@ Check_Solvable.check_solvable loop (*  *)

  let time_bound transformation_type (l,t,l') scc program appr =
    TWN_Proofs.proof := FormattedString.Empty;
    let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in
    if Option.is_none opt then (
      let bound = Timeout.timed_run 5. (fun () ->
      (* We have not yet computed a (local) runtime bound. *)
      let loops_opt = SimpleCycle.find_loops (heuristic_for_cycle transformation_type) appr program scc (l,t,l') in
      if Option.is_some loops_opt then
        let cycle, loops = Option.get loops_opt in
        let upd_invariant_cand = List.map (Constraint.atom_list % TransitionLabel.invariant % Tuple3.second) cycle |> List.flatten in
        let local_bounds = List.map (fun (entry,(loop,aut)) -> entry, Automorphism.apply_to_bound (TWN_Complexity.complexity ~entry:(Option.some entry) upd_invariant_cand loop) aut) loops in
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
