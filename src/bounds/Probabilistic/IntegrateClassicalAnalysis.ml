open OurBase
open Bounds
open ProbabilisticProgramModules
open Approximation.Probabilistic
module MultiphaseRankingFunction = MultiphaseRankingFunction.Make (NonProbOverappr)
module UnliftedBound = UnliftedBounds.UnliftedTimeBound.Make (NonProbOverappr) (Bound)
open OverapprDirections

let nonprob_trans_to_prob_trans = Type_equal.(conv ProbabilisticPrograms.Equalities.trans_eq)
let prob_trans_to_nonprob_trans = Type_equal.(conv (sym ProbabilisticPrograms.Equalities.trans_eq))

let gts_to_nonprob_transs gts =
  GeneralTransitionSet.all_transitions gts |> NonProbOverappr.TransitionSet.map ~f:prob_trans_to_nonprob_trans


let get_timebound all_gts direction (class_appr, appr) t =
  let prob_trans = nonprob_trans_to_prob_trans t in
  match direction with
  | `ExpTime ->
      RealBound.to_intbound
      @@ ExpApproximation.timebound appr
           (Option.value_exn @@ GeneralTransitionSet.find_by_transition all_gts prob_trans)
  | `ClassTime -> ClassicalApproximation.timebound class_appr prob_trans


let get_sizebound all_gts direction (class_appr, appr) t var =
  let prob_trans = nonprob_trans_to_prob_trans t in
  match direction with
  | `ExpSize ->
      let gt = Option.value_exn (GeneralTransitionSet.find_by_transition all_gts prob_trans) in
      RealBound.to_intbound @@ ExpApproximation.sizebound appr (gt, Transition.target prob_trans) var
  | `ClassSize -> ClassicalApproximation.sizebound class_appr prob_trans var


(* If we have a classical time bound we use expected size bounds (otherwise vice versa).
   This is not necessarily optimal as there are cases where a classical size bound exists for some transition but no expected size bound for the encompassing general transition (if it contains multiple transitions).
   However, this is probably good enough.
   Moreover, at this point we don't know the bound the sizebounds will be substituted in.
   So when actually performing the substition one has to ensure to only substitute expected size bounds for linearly occuring variables to establish soundness. *)
let get_combined_bounds_both_directions ?proof all_gts apprs t =
  let classtime_bound = get_timebound all_gts `ClassTime apprs t in
  if Bound.is_finite classtime_bound then
    let dir = `ClassTimeExpSize in
    let get_expsize_bound = get_sizebound all_gts (size_direction dir) apprs t in
    (classtime_bound, get_expsize_bound, dir)
  else
    let dir = `ExpTimeClassSize in
    let exptime_bound = get_timebound all_gts (time_direction dir) apprs t in
    let get_classsize_bound = get_sizebound all_gts (size_direction dir) apprs t in
    (exptime_bound, get_classsize_bound, dir)


let get_timebound_and_sizebound_both_directions ?proof all_gts apprs =
  let add_to_proof t dir dir_to_string =
    Option.iter proof ~f:(fun proof ->
        ProofOutput.LocalProofOutput.add_to_proof proof
          FormattedString.(
            fun () ->
              mk_str_line @@ "Use " ^ dir_to_string dir ^ " for "
              ^ Transition.to_id_string_pretty (nonprob_trans_to_prob_trans t)))
  in
  let get_timebound t =
    let bound, _, dir = get_combined_bounds_both_directions ?proof all_gts apprs t in
    add_to_proof t dir (direction_to_string_time % time_direction);
    bound
  in
  let get_sizebound t =
    let _, get_size, dir = get_combined_bounds_both_directions ?proof all_gts apprs t in
    add_to_proof t dir (direction_to_string_size % size_direction);
    get_size
  in
  (get_timebound, get_sizebound)


let get_timebound_and_unbounded_vars_both_directions program_vars all_gts apprs =
  let get_timebound, get_sizebound = get_timebound_and_sizebound_both_directions all_gts apprs in
  let is_timebound_finite = Bound.is_finite % get_timebound in
  let unbounded_vars t = Set.filter program_vars ~f:(Bound.is_infinity % get_sizebound t) in
  (is_timebound_finite, unbounded_vars)


let improve_with_mprfs depth (program, all_gts) scc (class_appr, appr) =
  let all_unbounded_gts = Set.filter ~f:(not % ExpApproximation.is_time_bounded appr) scc in
  let all_trans_non_prob = gts_to_nonprob_transs scc in
  let unbounded_trans = GeneralTransitionSet.all_transitions all_unbounded_gts in

  let class_program = Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program in
  let program_vars = Program.input_vars program in
  let is_time_bounded, unbounded_vars =
    get_timebound_and_unbounded_vars_both_directions program_vars all_gts (class_appr, appr)
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
                 get_timebound_and_sizebound_both_directions ~proof:time_and_size_direction_proof all_gts
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
           List.map ~f:(fun (b, _) -> b) new_bounds_and_proofs |> RealBound.of_intbound % Bound.sum_list
         in
         ProofOutput.add_to_proof_with_format
           FormattedString.(
             fun format ->
               let header =
                 mk_header_small @@ mk_str @@ "Computed expected time bound for "
                 ^ GeneralTransition.to_id_string_pretty gt
                 ^ " with MPRF"
               in
               let new_bound =
                 mk_str_line @@ "Obtained bound " ^ RealBound.to_string ~pretty:true new_bound
               in
               let mprf_proofs =
                 List.map
                   ~f:(fun (_, f) -> reduce_header_sizes ~levels_to_reduce:1 (f format))
                   new_bounds_and_proofs
               in
               header <> new_bound <> mappend mprf_proofs);
         MaybeChanged.changed (ExpApproximation.add_timebound new_bound gt appr))


let improve ~mprf_depth (program, all_gts) scc class_appr appr =
  (* improve with multiphase ranking functions if requested *)
  let appr =
    match mprf_depth with
    | Some mprf_depth -> improve_with_mprfs mprf_depth (program, all_gts) scc (class_appr, appr)
    | None -> MaybeChanged.same appr
  in
  appr
