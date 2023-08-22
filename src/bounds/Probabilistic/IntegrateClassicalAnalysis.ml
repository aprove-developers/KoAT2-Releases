open OurBase
open Bounds
open ProbabilisticProgramModules
open Approximation.Probabilistic
module MultiphaseRankingFunction = MultiphaseRankingFunction.Make (NonProbOverappr)
module TWN = TWN.Make (NonProbOverappr)
module UnliftedBound = UnliftedBounds.UnliftedTimeBound.Make (NonProbOverappr) (Bound)
open OverapprDirections

let nonprob_trans_to_prob_trans = Type_equal.(conv ProbabilisticPrograms.Equalities.trans_eq)
let prob_trans_to_nonprob_trans = Type_equal.(conv (sym ProbabilisticPrograms.Equalities.trans_eq))

let gts_to_nonprob_transs gts =
  GeneralTransitionSet.all_transitions gts |> NonProbOverappr.TransitionSet.map ~f:prob_trans_to_nonprob_trans


let get_timebound direction (class_appr, appr) t =
  let prob_trans = nonprob_trans_to_prob_trans t in
  match direction with
  | `ExpTime -> RationalBound.to_intbound @@ ExpApproximation.timebound appr (Transition.gt prob_trans)
  | `ClassTime -> ClassicalApproximation.timebound class_appr prob_trans


let get_sizebound direction (class_appr, appr) t var =
  let prob_trans = nonprob_trans_to_prob_trans t in
  match direction with
  | `ExpSize ->
      let gt = Transition.gt prob_trans in
      RationalBound.to_intbound @@ ExpApproximation.sizebound appr (gt, Transition.target prob_trans) var
  | `ClassSize -> ClassicalApproximation.sizebound class_appr prob_trans var


(* If we have a classical time bound we use expected size bounds (otherwise vice versa).
   This is not necessarily optimal as there are cases where a classical size bound exists for some transition but no expected size bound for the encompassing general transition (if it contains multiple transitions).
   However, this is probably good enough.
   Moreover, at this point we don't know the bound the sizebounds will be substituted in.
   So when actually performing the substition one has to ensure to only substitute expected size bounds for linearly occuring variables to establish soundness. *)
let get_combined_bounds_both_directions ?proof apprs t =
  let classtime_bound = get_timebound `ClassTime apprs t in
  if Bound.is_finite classtime_bound then
    let dir = `ClassTimeExpSize in
    let get_expsize_bound = get_sizebound (size_direction dir) apprs t in
    (classtime_bound, get_expsize_bound, dir)
  else
    let dir = `ExpTimeClassSize in
    let exptime_bound = get_timebound (time_direction dir) apprs t in
    let get_classsize_bound = get_sizebound (size_direction dir) apprs t in
    (exptime_bound, get_classsize_bound, dir)


let get_timebound_and_sizebound_both_directions ?proof apprs =
  let add_to_proof t dir dir_to_string =
    Option.iter proof ~f:(fun proof ->
        ProofOutput.LocalProofOutput.add_to_proof proof
          FormattedString.(
            fun () ->
              mk_str_line @@ "Use " ^ dir_to_string dir ^ " for "
              ^ Transition.to_id_string_pretty (nonprob_trans_to_prob_trans t)))
  in
  let get_timebound t =
    let bound, _, dir = get_combined_bounds_both_directions ?proof apprs t in
    add_to_proof t dir (direction_to_string_time % time_direction);
    bound
  in
  let get_sizebound t =
    let _, get_size, dir = get_combined_bounds_both_directions ?proof apprs t in
    add_to_proof t dir (direction_to_string_size % size_direction);
    get_size
  in
  (get_timebound, get_sizebound)


let get_timebound_and_unbounded_vars_both_directions program_vars apprs =
  let get_timebound, get_sizebound = get_timebound_and_sizebound_both_directions apprs in
  let is_timebound_finite = Bound.is_finite % get_timebound in
  let unbounded_vars t = Set.filter program_vars ~f:(Bound.is_infinity % get_sizebound t) in
  (is_timebound_finite, unbounded_vars)


let improve_with_mprfs depth program scc (class_appr, appr) =
  let all_unbounded_gts = Set.filter ~f:(not % ExpApproximation.is_time_bounded appr) scc in
  let all_trans_non_prob = gts_to_nonprob_transs scc in
  let unbounded_trans = GeneralTransitionSet.all_transitions all_unbounded_gts in

  let class_program = Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program in
  let program_vars = Program.input_vars program in
  let is_time_bounded, unbounded_vars =
    get_timebound_and_unbounded_vars_both_directions program_vars (class_appr, appr)
  in
  let find_mprf t =
    let nonprob_trans = prob_trans_to_nonprob_trans t in
    let find_of_depth depth =
      MultiphaseRankingFunction.find_scc `Time class_program is_time_bounded unbounded_vars all_trans_non_prob
        depth nonprob_trans
    in
    Sequence.range ~start:`inclusive ~stop:`inclusive 1 depth
    |> Sequence.map ~f:find_of_depth |> Util.cat_maybes_sequence |> Sequence.hd
  in
  let unbounded_trans_arr = Set.to_array unbounded_trans in
  let mprf_map =
    Parmap.array_parmap find_mprf unbounded_trans_arr
    |> Array.zip_exn unbounded_trans_arr
    |> Array.filter_map ~f:(fun (t, mprf) -> Option.map ~f:(fun mprf -> (t, mprf)) mprf)
    |> Array.to_list
    |> Map.of_alist_exn (module Transition)
  in

  let unbounded_gts_with_mprfs =
    let open OptionMonad in
    Set.to_list all_unbounded_gts
    |> List.map ~f:(fun gt ->
           let+ mprfs =
             Set.to_list (GeneralTransition.transitions gt)
             |> List.map ~f:(Map.find mprf_map)
             |> OptionMonad.sequence
           in
           (gt, mprfs))
    |> Util.cat_maybes
  in
  Sequence.of_list unbounded_gts_with_mprfs
  |> MaybeChanged.fold_sequence ~init:appr ~f:(fun appr (gt, mprfs) ->
         let new_bounds_and_proofs =
           List.map mprfs ~f:(fun mprf ->
               let time_and_size_direction_proof = ProofOutput.LocalProofOutput.create () in
               let get_timebound, get_sizebound =
                 get_timebound_and_sizebound_both_directions ~proof:time_and_size_direction_proof
                   (class_appr, appr)
               in
               let unlifted = MultiphaseRankingFunction.to_unlifted_bound class_program mprf in
               let new_bound, compute_proof =
                 UnliftedBound.lift_and_get_proof ~get_timebound ~get_sizebound unlifted
               in
               let compute_proof format =
                 FormattedString.(
                   compute_proof format
                   <> mk_block (ProofOutput.LocalProofOutput.get_proof time_and_size_direction_proof format))
               in
               (new_bound, compute_proof))
         in
         let new_bound =
           List.map ~f:(fun (b, _) -> b) new_bounds_and_proofs |> RationalBound.of_intbound % Bound.sum_list
         in
         ProofOutput.add_to_proof_with_format
           FormattedString.(
             fun format ->
               let header =
                 mk_header_small @@ mk_str @@ "Computed expected time bound for "
                 ^ GeneralTransition.to_id_string_pretty gt
                 ^ " with MPRF"
               in
               (* Due to the heuristic the bound should always be finite so we don't need to explicitly check here *)
               let new_bound =
                 mk_str_line @@ "Obtained bound " ^ RationalBound.to_string ~pretty:true new_bound
               in
               let mprf_proofs =
                 List.map
                   ~f:(fun (_, f) -> reduce_header_sizes ~levels_to_reduce:1 (f format))
                   new_bounds_and_proofs
               in
               header <> new_bound <> mappend mprf_proofs);
         MaybeChanged.changed (ExpApproximation.add_timebound new_bound gt appr))


(** We want to compare TWN Loops by the set of their handed transitions *)
module TWNLoopWithProof = struct
  module Inner = struct
    type t = TWN.twn_loop ProofOutput.LocalProofOutput.with_proof

    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

    let compare twn_loop1 twn_loop2 =
      let get_cycle_set (l : t) =
        NonProbOverappr.TransitionSet.of_list @@ Tuple2.first (ProofOutput.LocalProofOutput.result l)
      in
      Set.compare_direct (get_cycle_set twn_loop1) (get_cycle_set twn_loop2)
  end

  include Inner
  include Comparator.Make (Inner)
end

type twn_loop_with_proof_set = (TWNLoopWithProof.t, TWNLoopWithProof.comparator_witness) Set.t

(* in bounds from_twn_loops we only store finite bounds (and functions to compute corresponding proofs) *)
type twn_state = {
  bounds_from_twn_loops :
    (Transition.t, Bound.t * (Formatter.format -> FormattedString.t), Transition.comparator_witness) Map.t;
  remaining_twn_loops : twn_loop_with_proof_set;
}

let twn_state_to_string twn_state =
  Printf.sprintf "bounds_from_twn_loops: %s\nremaining_twn_loops: %s\n"
    (Map.to_sequence twn_state.bounds_from_twn_loops
    |> Util.sequence_to_string ~f:(fun (t, (b, _)) ->
           Transition.to_id_string_pretty t ^ ": " ^ Bound.to_string b))
    (Set.to_sequence twn_state.remaining_twn_loops
    |> Util.sequence_to_string ~f:(fun l_w_p ->
           let cycle, _ = ProofOutput.LocalProofOutput.result l_w_p in
           Sequence.of_list cycle |> Util.sequence_to_string ~f:NonProbOverappr.Transition.to_id_string_pretty)
    )


let all_already_bounded_trans twn_state = Map.keys twn_state.bounds_from_twn_loops |> TransitionSet.of_list

let empty_twn_state =
  {
    bounds_from_twn_loops = Map.empty (module Transition);
    remaining_twn_loops = Set.empty (module TWNLoopWithProof);
  }


let initial_twn_state twn_conf program scc =
  let class_program = Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program in
  let scc_nonprob = gts_to_nonprob_transs scc in
  let all_loops = TWN.find_all_possible_loops_for_scc twn_conf scc_nonprob class_program in
  {
    bounds_from_twn_loops = Map.empty (module Transition);
    remaining_twn_loops = Set.of_list (module TWNLoopWithProof) all_loops;
  }


(** At this point we cannot distinguish between variables that occur linearly or non-linearly in the resulting bound.
    This is however not unsound, as [TWN.finite_bound_possible_if_terminating_with_combined_bounds] only uses the provided [get_combined_bounds] as a heuristic and does not perform any substitution/overapproximation whatsoever. *)
let twn_heuristic apprs twn_loop =
  TWN.finite_bound_possible_if_terminating_with_combined_bounds twn_loop ~get_combined_bounds:(fun t ->
      let timebound, get_sizebound, _ = get_combined_bounds_both_directions apprs t in
      (timebound, get_sizebound))


let handled_transs_of_twn_loop_with_proof twn_loop_with_proof =
  let cycle, _ = ProofOutput.LocalProofOutput.result twn_loop_with_proof in
  Sequence.of_list cycle |> Sequence.map ~f:nonprob_trans_to_prob_trans


let handled_transs_of_twn_loops_with_proofs twn_loops_with_proofs =
  Sequence.map ~f:handled_transs_of_twn_loop_with_proof (Sequence.of_list twn_loops_with_proofs)
  |> Sequence.join |> TransitionSet.of_sequence


(** Here we build a map which local twn bounds are overapproximated in which direction/manner *)
let get_time_and_size_bounds_for_unlifted_twn_bounds ?proof apprs unlifted_bound =
  let entry_measure_map = UnliftedBound.entry_transitions_measure unlifted_bound in
  let time_and_size_bounds_map =
    Map.fold entry_measure_map
      ~init:(Map.empty (module NonProbOverappr.Transition))
      ~f:(fun ~key ~data t_and_s_map ->
        let classtime_bound = get_timebound `ClassTime apprs key in
        if Bound.is_finite classtime_bound then
          if Bound.is_linear data then
            let get_size = get_sizebound `ExpSize apprs key in
            Map.add_exn t_and_s_map ~key ~data:(classtime_bound, get_size, `ClassTimeExpSize)
          else
            let get_size = get_sizebound `ClassSize apprs key in
            Map.add_exn t_and_s_map ~key ~data:(classtime_bound, get_size, `ClassTimeClassSize)
        else
          let exptime_bound = get_timebound `ExpTime apprs key in
          let get_classsize_bound = get_sizebound `ClassSize apprs key in
          Map.add_exn t_and_s_map ~key ~data:(exptime_bound, get_classsize_bound, `ExpTimeClassSize))
  in
  let add_to_proof proof t dir =
    ProofOutput.LocalProofOutput.add_to_proof proof
      FormattedString.(
        fun () ->
          mk_str_line @@ "Use " ^ direction_to_string dir ^ " for "
          ^ Transition.to_id_string_pretty (nonprob_trans_to_prob_trans t))
  in
  let get_timebound t =
    let bound, _, _ = Map.find_exn time_and_size_bounds_map t in
    bound
  in
  let get_sizebound t =
    let _, get_size, _ = Map.find_exn time_and_size_bounds_map t in
    fun v -> get_size v
  in
  (* add used directions to proof *)
  Option.iter proof ~f:(fun proof ->
      Map.iteri time_and_size_bounds_map ~f:(fun ~key ~data ->
          let _, _, dir = data in
          add_to_proof proof key dir));
  (get_timebound, get_sizebound)


(** Compute timebound for transition from single twn loop *)
let compute_timebounds_from_loop_with_proof twn_state apprs loop_with_proof =
  let all_handled_trans =
    TWN.handled_transitions (ProofOutput.LocalProofOutput.result loop_with_proof)
    |> TransitionSet.map ~f:nonprob_trans_to_prob_trans
  in
  let newly_handled_trans = Set.diff all_handled_trans (all_already_bounded_trans !twn_state) in
  if not (Set.is_empty newly_handled_trans) then
    let unlifted = TWN.to_unlifted_bounds loop_with_proof in
    let time_and_size_proofs = ProofOutput.LocalProofOutput.create () in
    let get_timebound, get_sizebound =
      get_time_and_size_bounds_for_unlifted_twn_bounds ~proof:time_and_size_proofs apprs unlifted
    in
    let new_bound, compute_proof = UnliftedBound.lift_and_get_proof ~get_timebound ~get_sizebound unlifted in
    let compute_proof format =
      FormattedString.(
        compute_proof format <> mk_block (ProofOutput.LocalProofOutput.get_proof time_and_size_proofs format))
    in
    if Bound.is_finite new_bound then
      let bounds_from_twn_loops =
        Set.fold newly_handled_trans ~init:!twn_state.bounds_from_twn_loops ~f:(fun map t ->
            Map.add_exn map ~key:t ~data:(new_bound, compute_proof))
      in
      twn_state := { !twn_state with bounds_from_twn_loops }


let improve_with_twn_loops twn_state (class_appr, appr) unbounded_gts loops_with_proofs =
  List.iter loops_with_proofs ~f:(compute_timebounds_from_loop_with_proof twn_state (class_appr, appr));
  Set.to_sequence unbounded_gts
  |> Sequence.filter_map ~f:(fun gt ->
         let open OptionMonad in
         (* We only store finite bounds in twn_state *)
         let bounds_with_proofs =
           Set.to_list (GeneralTransition.transitions gt)
           |> List.map ~f:(fun t ->
                  let+ b_w_p = Map.find !twn_state.bounds_from_twn_loops t in
                  (t, b_w_p))
           |> OptionMonad.sequence
         in
         let+ transs_with_bounds_with_proofs = bounds_with_proofs in
         let b =
           Sequence.of_list transs_with_bounds_with_proofs
           |> Sequence.map ~f:(fun (_, (b, _)) -> RationalBound.of_intbound b)
           |> RationalBound.sum
         in
         ProofOutput.add_to_proof_with_format
           FormattedString.(
             fun format ->
               mk_str_header_small
                 ("Found new bound for " ^ GeneralTransition.to_id_string_pretty gt ^ " with TWN")
               <> mappend
                    (List.map transs_with_bounds_with_proofs ~f:(fun (t, (b, proof)) ->
                         reduce_header_sizes ~levels_to_reduce:2 (proof format))));
         (gt, b))
  |> MaybeChanged.fold_sequence ~init:appr ~f:(fun appr (gt, b) ->
         MaybeChanged.changed @@ ExpApproximation.add_timebound b gt appr)


let improve_with_twn twn_state scc (class_appr, appr) =
  let improve_step appr =
    let unbounded_gts = Set.filter ~f:(not % ExpApproximation.is_time_bounded appr) scc in
    let unbounded_gt_to_twn_loops_map =
      let partially_handled_unbounded_gts loop_with_proof =
        handled_transs_of_twn_loop_with_proof loop_with_proof
        |> Sequence.filter_map ~f:(fun t ->
               let gt = Transition.gt t in
               Option.some_if (Set.mem unbounded_gts gt) gt)
        |> GeneralTransitionSet.of_sequence
      in
      Set.fold !twn_state.remaining_twn_loops
        ~init:(Map.empty (module GeneralTransition))
        ~f:(fun map loop_with_proof ->
          Set.fold (partially_handled_unbounded_gts loop_with_proof) ~init:map ~f:(fun map gt ->
              Map.add_multi map ~key:gt ~data:loop_with_proof))
      |> Map.filteri ~f:(fun ~key ~data ->
             Set.is_subset (GeneralTransition.transitions key)
               ~of_:
                 (Set.union
                    (handled_transs_of_twn_loops_with_proofs data)
                    (all_already_bounded_trans !twn_state)))
    in
    let gt_to_improve_with_loops =
      Map.to_sequence unbounded_gt_to_twn_loops_map
      |> Sequence.find_map ~f:(fun (gt, loops_with_proofs) ->
             let all_new_boundable_loops =
               List.filter
                 ~f:(twn_heuristic (class_appr, appr) % ProofOutput.LocalProofOutput.result)
                 loops_with_proofs
             in
             let all_new_boundable_trans = handled_transs_of_twn_loops_with_proofs all_new_boundable_loops in
             if
               Set.is_subset (GeneralTransition.transitions gt)
                 ~of_:(Set.union all_new_boundable_trans (all_already_bounded_trans !twn_state))
             then
               Some (gt, all_new_boundable_loops)
             else
               None)
    in
    match gt_to_improve_with_loops with
    | None -> MaybeChanged.same appr
    | Some (gt, loops_with_proofs) ->
        let appr_mc = improve_with_twn_loops twn_state (class_appr, appr) unbounded_gts loops_with_proofs in
        let remaining_twn_loops =
          let all_considered_cycles = Set.of_list (module TWNLoopWithProof) loops_with_proofs in
          let all_twn_loops_in_map =
            Map.to_sequence unbounded_gt_to_twn_loops_map
            |> Sequence.map ~f:(Sequence.of_list % Tuple2.second)
            |> Sequence.join
            |> Set.of_sequence (module TWNLoopWithProof)
          in
          (* We give up computing a new time bound for gt, so remove it from the remaining loops *)
          Set.diff all_twn_loops_in_map all_considered_cycles
        in
        twn_state := { !twn_state with remaining_twn_loops };
        appr_mc
  in
  Util.find_fixpoint_mc improve_step appr


let improve ~twn:(twn_state, twn_conf) ~mprf_depth program scc (class_appr, appr) =
  let open MaybeChanged.Monad in
  (* improve with multiphase ranking functions if requested *)
  let appr =
    match mprf_depth with
    | Some mprf_depth -> improve_with_mprfs mprf_depth program scc (class_appr, appr)
    | None -> MaybeChanged.same appr
  in
  (* improve with twn if requested *)
  let appr =
    match twn_conf with
    | Some twn_conf ->
        let* appr = appr in
        improve_with_twn twn_state scc (class_appr, appr)
    | None -> appr
  in
  appr
