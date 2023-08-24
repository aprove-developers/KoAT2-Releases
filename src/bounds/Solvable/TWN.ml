open Automorphism
open Batteries
open Bounds
open FormattedString

let logger = Logging.(get Twn)

type configuration = [ `NoTransformation | `Transformation ]

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (PM)
  module Check_TWN = Check_TWN.Make (PM)
  module EliminateNonContributors = EliminateNonContributors.Make (PM)
  module TWN_Complexity = TWN_Complexity.Make (PM)
  module TWN_Termination = TWN_Termination.Make (PM)
  module SimpleCycle = SimpleCycle.Make (PM)
  module Check_Solvable = Check_Solvable.Make (PM)
  module TimeBoundTable = Hashtbl.Make (Transition)
  module UnliftedTimeBound = UnliftedBounds.UnliftedTimeBound.Make (PM) (Bound)

  (* Keys: transition, values: bounds of entry transitions. *)
  let termination_table : (Transition.t * bool) list TimeBoundTable.t = TimeBoundTable.create 10

  (* Internal memoization: The idea is to use this cache if we applied cfr and
     1) delete it and use the original cache if we get a timeout or
     2) if the analysis of the unrolled scc is completed successfully use this cache as the main memory.
     TODO Currently, we just reset the cache. *)
  let reset_cfr () = TimeBoundTable.clear termination_table

  let complete_proofs twn_proofs cycle ~get_timebound ~get_sizebound entry_measure_map lifted_bound =
    let for_entry_and_local_bound (entry, local_bound) =
      let bound_with_sizebound = Bound.substitute_f (get_sizebound entry) local_bound in
      Bound.mul (get_timebound entry) bound_with_sizebound
      |> tap @@ fun b ->
         Logger.log logger Logger.INFO (fun () ->
             ( "lift",
               Bound.vars local_bound |> Base.Set.to_list
               |> List.map (fun v ->
                      ( "t: " ^ Transition.to_id_string entry ^ ", yvar: " ^ Var.to_string v,
                        get_sizebound entry v |> Bound.to_string ~pretty:true )) ));
         Logger.log logger Logger.INFO (fun () ->
             ( "lift",
               [ ("RB of entry", get_timebound entry |> Bound.to_string); ("Result", Bound.to_string b) ] ));
         ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
             mk_str_header_small
               ("TWN - Lifting for " ^ TransitionSet.to_id_string cycle ^ " of "
               ^ Bound.to_string ~pretty:true local_bound)
             <> (mk_str_line ("relevant size-bounds w.r.t. " ^ Transition.to_id_string_pretty entry ^ ":")
                <> (Bound.vars local_bound |> Base.Set.to_list
                   |> List.map (fun v ->
                          Var.to_string ~pretty:true v ^ ": "
                          ^ (get_sizebound entry v |> Bound.to_string ~pretty:true))
                   |> List.map mk_str_line |> mappend)
                <> mk_str_line
                     ("Runtime-bound of t"
                     ^ (Transition.id entry |> Util.natural_to_subscript)
                     ^ ": "
                     ^ (get_timebound entry |> Bound.to_string ~pretty:true))
                <> mk_str_line ("Results in: " ^ Bound.to_string ~pretty:true b)))
    in
    OurBase.Map.iteri ~f:(fun ~key ~data -> ignore (for_entry_and_local_bound (key, data))) entry_measure_map;
    ProofOutput.add_to_proof_with_format (ProofOutput.LocalProofOutput.get_proof twn_proofs)


  type twn_loop = SimpleCycle.twn_loop

  let handled_transitions ((cycle, _) : twn_loop) = TransitionSet.of_list cycle

  let requirement_for_cycle conf loop =
    match conf with
    | `NoTransformation -> Check_TWN.check_twn loop
    | `Transformation -> Option.is_some @@ Check_Solvable.check_solvable loop


  let finite_bound_possible_if_terminating_with_combined_bounds ~get_combined_bounds (loop : twn_loop) =
    let loop_transitions, entries = loop in
    let loop_transitions = TransitionSet.of_list loop_transitions in
    let contributors = EliminateNonContributors.compute_contributors loop_transitions in
    OurBase.List.for_all entries ~f:(fun (entry_trans, _) ->
        let timebound, get_sizebound = get_combined_bounds entry_trans in
        Bound.is_finite timebound && OurBase.Set.for_all contributors ~f:(Bound.is_finite % get_sizebound))


  let finite_bound_possible_if_terminating ~get_timebound ~get_sizebound (loop : twn_loop) =
    finite_bound_possible_if_terminating_with_combined_bounds
      ~get_combined_bounds:(fun t -> (get_timebound t, get_sizebound t))
      loop


  let find_all_possible_loops_for_scc conf scc program :
      SimpleCycle.twn_loop ProofOutput.LocalProofOutput.with_proof List.t =
    let open! OurBase in
    let create_proof trans =
      let twn_proofs = ProofOutput.LocalProofOutput.create () in
      ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
          mk_str_header_big @@ "TWN: " ^ Transition.to_id_string_pretty trans);
      twn_proofs
    in

    let rec loop scc =
      match Set.choose scc with
      | None -> []
      | Some trans ->
          let all_cycles =
            SimpleCycle.find_all_loops (create_proof trans) (requirement_for_cycle conf) program scc trans
          in
          let scc' = Set.remove scc trans in
          List.append all_cycles (loop scc')
    in
    loop scc


  let to_unlifted_bounds (twn_loop : twn_loop ProofOutput.LocalProofOutput.with_proof) =
    let twn_proofs, (cycle, loops) = ProofOutput.LocalProofOutput.(proof twn_loop, result twn_loop) in
    let cycle_set = TransitionSet.of_list cycle in
    let local_bounds =
      List.map
        (fun (entry, (loop, aut)) ->
          ( entry,
            Automorphism.apply_to_bound
              (TWN_Complexity.complexity twn_proofs ~entry:(Option.some entry) loop)
              aut ))
        loops
    in
    let complete_proofs = complete_proofs twn_proofs cycle_set in
    UnliftedTimeBound.mk ~measure_decr_transitions:cycle_set
      ~compute_proof:
        (Option.some (fun ~get_timebound ~get_sizebound entry_map res_bound format ->
             complete_proofs ~get_timebound ~get_sizebound entry_map res_bound;
             ProofOutput.LocalProofOutput.get_proof twn_proofs format))
      (OurBase.Map.of_alist_exn (module Transition) local_bounds)


  let terminates conf (l, t, l') scc program appr =
    let twn_proofs = ref (ProofOutput.LocalProofOutput.create ()) in
    ProofOutput.LocalProofOutput.add_to_proof !twn_proofs (fun () ->
        mk_str_header_big @@ "TWN: " ^ Transition.to_id_string_pretty (l, t, l'));
    let compute_new_bound =
      let bound =
        Timeout.timed_run 5. (fun () ->
            (* Local termination was not proven yet. *)
            let compute_termination (cycle, loops) =
              let is_bounded entry loop =
                TWN_Termination.termination !twn_proofs ~entry:(Option.some entry) loop
              in
              let local_bounds = List.map (fun (entry, (loop, _)) -> (entry, is_bounded entry loop)) loops in
              List.iter (fun t -> TimeBoundTable.add termination_table t local_bounds) cycle;
              List.for_all Tuple2.second local_bounds
            in
            let handle_missing_loops =
              TimeBoundTable.add termination_table (l, t, l') [ ((l, t, l'), false) ]
            in
            (* find all loops copies the proof output *)
            SimpleCycle.find_all_loops !twn_proofs (requirement_for_cycle conf) program scc (l, t, l')
            |> Base.List.hd
            |> Option.map (fun with_proof ->
                   ProofOutput.LocalProofOutput.(with_proof.result, with_proof.proof))
            (*If no simple loops were found we cannot prove termination*)
            |> Option.map_default
                 (fun (r, twn_proofs') ->
                   twn_proofs := twn_proofs';
                   compute_termination r)
                 (handle_missing_loops;
                  false))
      in
      (* In case no bound was computed this maps to false otherwise to the bound*)
      Option.map_default Tuple2.first false bound
    in
    TimeBoundTable.find_option termination_table (l, t, l')
    (*If a bound was computed we check for finiteness*)
    |> Option.map_default (List.for_all Tuple2.second) compute_new_bound
    |> tap (fun terminates ->
           if terminates && (Bound.is_infinity @@ Approximation.timebound appr (l, t, l')) then
             ProofOutput.add_to_proof_with_format (ProofOutput.LocalProofOutput.get_proof !twn_proofs))
end
