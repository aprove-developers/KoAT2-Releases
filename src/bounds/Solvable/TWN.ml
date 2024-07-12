open OurBase
open FormattedString

let logger = Logging.(get Twn)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)
  module EliminateNonContributors = EliminateNonContributors.Make (PM)
  module TWN_Complexity = TWN_Complexity.Make (Bound) (PM)
  module SimpleCycle = SimpleCycle.Make (Bound) (PM)
  module UnliftedTimeBound = UnliftedBounds.UnliftedTimeBound.Make (PM) (Bound)
  module Loop = Loop.Make (Bound) (PM)

  let complete_proofs twn_proofs cycle ~get_timebound ~get_sizebound entry_measure_map lifted_bound =
    let for_entry_and_local_bound (entry, local_bound) =
      let bound_with_sizebound = Bound.substitute_f (get_sizebound entry) local_bound in
      Bound.mul (get_timebound entry) bound_with_sizebound
      |> tap @@ fun b ->
         Logger.log logger Logger.INFO (fun () ->
             ( "lift",
               Bound.vars local_bound |> Set.to_list
               |> List.map ~f:(fun v ->
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
                <> (Bound.vars local_bound |> Set.to_list
                   |> List.map ~f:(fun v ->
                          Var.to_string ~pretty:true v ^ ": "
                          ^ (get_sizebound entry v |> Bound.to_string ~pretty:true))
                   |> List.map ~f:mk_str_line |> mappend)
                <> mk_str_line
                     ("Runtime-bound of t"
                     ^ (Transition.id entry |> Util.natural_to_subscript)
                     ^ ": "
                     ^ (get_timebound entry |> Bound.to_string ~pretty:true))
                <> mk_str_line ("Results in: " ^ Bound.to_string ~pretty:true b)))
    in
    Map.iteri ~f:(fun ~key ~data -> ignore (for_entry_and_local_bound (key, data))) entry_measure_map;
    ProofOutput.add_to_proof_with_format (ProofOutput.LocalProofOutput.get_proof twn_proofs)


  type twn_loop = SimpleCycle.twn_loop

  let handled_transitions ((cycle, _) : twn_loop) = TransitionSet.of_list cycle

  let finite_bound_possible_if_terminating_with_combined_bounds ~get_combined_bounds (loop : twn_loop) =
    let loop_transitions, entries = loop in
    let loop_transitions = TransitionSet.of_list loop_transitions in
    let contributors = EliminateNonContributors.compute_contributors loop_transitions in
    List.for_all entries ~f:(fun (entry_trans, _) ->
        let timebound, get_sizebound = get_combined_bounds entry_trans in
        Bound.is_finite timebound && Set.for_all contributors ~f:(Bound.is_finite % get_sizebound))


  let finite_bound_possible_if_terminating ~get_timebound ~get_sizebound (loop : twn_loop) =
    finite_bound_possible_if_terminating_with_combined_bounds
      ~get_combined_bounds:(fun t -> (get_timebound t, get_sizebound t))
      loop


  let find_all_possible_loops_for_scc requirement_for_cycle scc program :
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
            SimpleCycle.find_all_loops (create_proof trans)
              (fun loop _entry_trans -> requirement_for_cycle loop)
              program scc trans
          in
          let scc' = Set.remove scc trans in
          List.append all_cycles (loop scc')
    in
    loop scc


  let to_unlifted_bounds ?(unsolvable = false) (twn_loop : twn_loop ProofOutput.LocalProofOutput.with_proof) =
    let twn_proofs, (cycle, loops) = ProofOutput.LocalProofOutput.(proof twn_loop, result twn_loop) in
    let cycle_set = TransitionSet.of_list cycle in
    let local_bounds =
      List.map
        ~f:(fun (entry, loop) ->
          (entry, TWN_Complexity.complexity ~unsolvable twn_proofs ~entry:(Option.some entry) loop))
        loops
    in
    let complete_proofs = complete_proofs twn_proofs cycle_set in
    UnliftedTimeBound.mk ~measure_decr_transitions:cycle_set
      ~compute_proof:
        (Option.some (fun ~get_timebound ~get_sizebound entry_map res_bound format ->
             complete_proofs ~get_timebound ~get_sizebound entry_map res_bound;
             ProofOutput.LocalProofOutput.get_proof twn_proofs format))
      (Map.of_alist_exn (module Transition) local_bounds)
end
