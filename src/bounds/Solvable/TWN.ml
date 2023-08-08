open Automorphism
open Atoms
open Batteries
open Bounds
open Constraints
open Formulas
open FormattedString
open PolyExponential
open Polynomials
open ProgramModules

let logger = Logging.(get Twn)

type configuration = [`NoTransformation | `Transformation]
module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Approximation = Approximation.MakeForClassicalAnalysis(PM)
  module Check_TWN = Check_TWN.Make(PM)
  module TWN_Complexity = TWN_Complexity.Make(PM)
  module TWN_Termination = TWN_Termination.Make(PM)
  module SimpleCycle = SimpleCycle.Make(PM)
  module Check_Solvable = Check_Solvable.Make(PM)
  module TimeBoundTable = Hashtbl.Make(Transition)

  module UnliftedTimeBound = UnliftedBounds.UnliftedTimeBound.Make(PM)(Bound)

  (* Keys: transition, values: bounds of entry transitions. *)
  let time_bound_table: UnliftedTimeBound.t Option.t TimeBoundTable.t = TimeBoundTable.create 10
  let termination_table: (Transition.t * bool) list TimeBoundTable.t = TimeBoundTable.create 10
  (** Internal memoization: The idea is to use this cache if we applied cfr and
    1) delete it and use the original cache if we get a timeout or
    2) if the analysis of the unrolled scc is completed successfully use this cache as the main memory.
    TODO Currently, we just reset the cache. *)
  let reset_cfr () =
    TimeBoundTable.clear time_bound_table;
    TimeBoundTable.clear termination_table

  let add_to_proof_hook twn_proofs t ~get_timebound ~get_sizebound entry_measure_map lifted_bound =
    let for_entry_and_local_bound (entry,local_bound) =
      let bound_with_sizebound = Bound.substitute_f (get_sizebound entry) local_bound in
      Bound.mul (get_timebound entry) bound_with_sizebound
      |> tap @@ fun b ->
      Logger.log logger Logger.INFO (fun () -> "lift",
                                              Bound.vars local_bound
                                              |> Base.Set.to_list
                                              |> List.map (fun v -> ("t: " ^ (Transition.to_id_string entry)  ^ ", yvar: " ^ Var.to_string v) , (get_sizebound entry v |> Bound.to_string ~pretty:true)));
      Logger.log logger Logger.INFO (fun () -> "lift", [("RB of entry", get_timebound entry |> Bound.to_string); ("Result", Bound.to_string b)]);
      ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
        (mk_str_header_small ("TWN - Lifting for " ^ (Transition.to_id_string_pretty t) ^ " of " ^ (Bound.to_string ~pretty:true local_bound))) <>
        (mk_str_line ("relevant size-bounds w.r.t. t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ":") <> (
            Bound.vars local_bound
            |> Base.Set.to_list
            |> List.map (fun v -> (Var.to_string ~pretty:true v) ^ ": " ^ (get_sizebound entry v |> Bound.to_string ~pretty:true))
            |> List.map mk_str_line
            |> mappend) <>
        mk_str_line ("Runtime-bound of t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ": " ^ (get_timebound entry |> Bound.to_string ~pretty:true)) <>
        mk_str ("Results in: " ^ (Bound.to_string ~pretty:true b))));
    in
    OurBase.Map.iteri
      ~f:(fun ~key ~data -> ignore (for_entry_and_local_bound (key,data)))
      entry_measure_map;
    ProofOutput.add_to_proof (fun () -> ProofOutput.LocalProofOutput.get_proof twn_proofs)

  let heuristic_for_cycle conf appr entry program loop = match conf with
    | `NoTransformation -> Check_TWN.check_twn loop && Approximation.is_time_bounded appr entry
    | `Transformation -> Option.is_some @@ Check_Solvable.check_solvable loop (*  *)

  let to_unlifted_bounds prev_part_of_proof trans cycle local_bounds =
    let cycle_set = TransitionSet.of_list cycle in
    UnliftedTimeBound.mk
      ~measure_decr_transitions:cycle_set
      ~hook:(Option.some @@ add_to_proof_hook prev_part_of_proof trans)
      (OurBase.Map.of_alist_exn (module Transition) local_bounds)

  let time_bound conf (l,t,l') scc program appr =
    let twn_proofs = ProofOutput.LocalProofOutput.create () in
    ProofOutput.LocalProofOutput.add_to_proof twn_proofs
      (fun () -> mk_str_header_big @@ "TWN: " ^ (Transition.to_id_string_pretty (l,t,l')));
    let bound =
      let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in
      if Option.is_none opt then (
        let unlifted_result = Timeout.timed_run 5. (fun () ->
        (* We have not yet computed a (local) runtime bound. *)
        let loops_opt = SimpleCycle.find_loops twn_proofs (heuristic_for_cycle conf) appr program scc (l,t,l') in
        if Option.is_some loops_opt then
          let cycle, loops = Option.get loops_opt in
          let local_bounds = List.map (fun (entry,(loop,aut)) -> entry, Automorphism.apply_to_bound (TWN_Complexity.complexity twn_proofs ~entry:(Option.some entry) loop) aut) loops in
          let unlifted_o = Some (to_unlifted_bounds twn_proofs (l,t,l') cycle local_bounds) in
          List.iter (fun t -> TimeBoundTable.add time_bound_table t unlifted_o) cycle;
          unlifted_o
        else (
          TimeBoundTable.add time_bound_table (l,t,l') None;
          None)) in
        if Option.is_some unlifted_result then
          Tuple2.first @@ Option.get unlifted_result
        else
          None
      ) else (
        (* We already have computed a (local) runtime bound and just lift it again.*)
        let unlifted_o = Option.get opt in
        unlifted_o
      )
    in
    bound

    let terminates conf (l,t,l') scc program appr =
      let twn_proofs = ProofOutput.LocalProofOutput.create () in
      ProofOutput.LocalProofOutput.add_to_proof twn_proofs
        (fun () -> mk_str_header_big @@ "TWN: " ^ (Transition.to_id_string_pretty (l,t,l')));
      let compute_new_bound =
        let bound = Timeout.timed_run 5. (fun () ->
          (* Local termination was not proven yet. *)
          let compute_termination (cycle, loops) =
            let is_bounded entry loop = TWN_Termination.termination twn_proofs ~entry:(Option.some entry) loop in
            let local_bounds = List.map (fun (entry,(loop,_)) -> entry, is_bounded entry loop) loops in
            List.iter (fun t -> TimeBoundTable.add termination_table t local_bounds) cycle;
            List.for_all Tuple2.second local_bounds in
          let handle_missing_loops = TimeBoundTable.add termination_table (l,t,l') [(l,t,l'),false] in
          SimpleCycle.find_loops twn_proofs (heuristic_for_cycle conf) appr program scc (l,t,l')
          (*If no simple loops were found we cannot prove termination*)
          |> Option.map_default compute_termination (handle_missing_loops; false)) in
        (* In case no bound was computed this maps to false otherwise to the bound*)
        Option.map_default Tuple2.first false bound
      in
      TimeBoundTable.find_option termination_table (l,t,l')
      (*If a bound was computed we check for finiteness*)
      |> Option.map_default (List.for_all Tuple2.second) compute_new_bound
      |> tap (fun terminates ->
        if terminates && (Bound.is_infinity @@ Approximation.timebound appr (l,t,l')) then (
          ProofOutput.add_to_proof (fun () -> ProofOutput.LocalProofOutput.get_proof twn_proofs));)
end
