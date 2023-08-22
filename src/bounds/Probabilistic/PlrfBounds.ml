open! OurBase
open Bounds
open ProbabilisticProgramModules
open Approximation.Probabilistic

let logger = Logging.(get ExpTime)

open OverapprDirections

let get_timebound direction (class_appr, appr) =
  match direction with
  | `ExpTime -> fun (gt, l) -> ExpApproximation.timebound appr gt
  | `ClassTime ->
      fun (gt, l) ->
        GeneralTransition.transitions gt |> Set.to_sequence
        |> Sequence.filter ~f:(Location.equal l % Transition.target)
        |> Sequence.map ~f:(ClassicalApproximation.timebound class_appr)
        |> Bound.sum |> RationalBound.of_intbound


let get_sizebound direction (class_appr, appr) =
  match direction with
  | `ExpSize -> ExpApproximation.sizebound appr
  | `ClassSize ->
      fun (gt, l) var ->
        GRV.to_probabilistic_rvs ((gt, l), var)
        |> Sequence.map ~f:(fun (t, v) -> ClassicalApproximation.sizebound class_appr t v)
        |> Bound.sum |> RationalBound.of_intbound


let get_combined_bounds_both_directions apprs (gt, l) =
  let classtime_bound = get_timebound `ClassTime apprs (gt, l) in
  if RationalBound.is_finite classtime_bound then
    let dir = `ClassTimeExpSize in
    let get_expsize_bound = get_sizebound (size_direction dir) apprs (gt, l) in
    (classtime_bound, get_expsize_bound, dir)
  else
    let dir = `ExpTimeClassSize in
    let exptime_bound = get_timebound (time_direction dir) apprs (gt, l) in
    let get_classsize_bound = get_sizebound (size_direction dir) apprs (gt, l) in
    (exptime_bound, get_classsize_bound, dir)


let get_timebound_and_sizebound_both_directions ?proof apprs =
  let add_to_proof (gt, l) dir dir_to_string =
    Option.iter proof ~f:(fun proof ->
        ProofOutput.LocalProofOutput.add_to_proof proof
          FormattedString.(
            fun () ->
              mk_str_line @@ "Use " ^ dir_to_string dir ^ " for entry point ("
              ^ GeneralTransition.to_id_string_pretty gt
              ^ "," ^ Location.to_string l ^ ")"))
  in
  let get_timebound (gt, l) =
    let bound, _, dir = get_combined_bounds_both_directions apprs (gt, l) in
    add_to_proof (gt, l) dir (direction_to_string_time % time_direction);
    bound
  in
  let get_sizebound (gt, l) =
    let _, get_size, dir = get_combined_bounds_both_directions apprs (gt, l) in
    add_to_proof (gt, l) dir (direction_to_string_size % size_direction);
    get_size
  in
  (get_timebound, get_sizebound)


let improve_with_plrf program (class_appr, appr) rank =
  let non_inc = Plrf.non_increasing rank in

  let incoming_gts = BoundsHelper.entry_gts program non_inc in
  let entry_locations = BoundsHelper.entry_locations_of_gts program non_inc incoming_gts in

  let entry_time_and_size_bound_proofs = ProofOutput.LocalProofOutput.create () in

  let new_bound =
    Set.to_sequence entry_locations
    |> Sequence.map ~f:(fun entry_loc ->
           let entry_gts_to_loc =
             Set.filter ~f:(flip Set.mem entry_loc % GeneralTransition.targets) incoming_gts
           in
           let get_timebound, get_sizebound =
             get_timebound_and_sizebound_both_directions ~proof:entry_time_and_size_bound_proofs
               (class_appr, appr)
           in

           let rank_size_bound v =
             Set.to_sequence entry_gts_to_loc
             |> Sequence.map ~f:(fun gt -> get_sizebound (gt, entry_loc) v)
             |> RationalBound.sum
           in
           let rank_at_loc = Plrf.rank rank entry_loc in
           (* Here we assume that rank_at_loc is linear as is always the case for PLRFs *)
           let rank_bounded =
             RationalBound.substitute_f rank_size_bound (RationalBound.of_poly rank_at_loc)
           in

           let inc_timebound =
             Set.to_sequence entry_gts_to_loc
             |> Sequence.map ~f:(fun gt -> get_timebound (gt, entry_loc))
             |> RationalBound.sum
           in
           RationalBound.(inc_timebound * rank_bounded))
    |> RationalBound.sum
  in
  Logger.log logger Logger.DEBUG (fun () ->
      ("improve_with_plrf", [ ("rank", Plrf.to_string rank); ("bound", RationalBound.to_string new_bound) ]));

  if RationalBound.is_finite new_bound then (
    ProofOutput.add_to_proof_with_format
      FormattedString.(
        fun fmt ->
          Plrf.compute_proof rank new_bound program fmt
          <> mk_block (ProofOutput.LocalProofOutput.get_proof entry_time_and_size_bound_proofs fmt));
    MaybeChanged.changed (ExpApproximation.add_timebound new_bound (Plrf.decreasing rank) appr))
  else
    MaybeChanged.same appr


let improve_timebounds_plrf ~compute_refined_plrfs program scc (class_appr, appr) :
    ExpApproximation.t MaybeChanged.t =
  let get_timebound, get_sizebound = get_timebound_and_sizebound_both_directions (class_appr, appr) in
  let is_exptime_bounded = RationalBound.is_finite % get_timebound in
  let unbounded_vars (gt, l) =
    Program.input_vars program
    |> Set.filter ~f:(RationalBound.is_infinity % ExpApproximation.sizebound appr (gt, l))
  in
  let find_plrfs refined =
    Set.filter ~f:(not % ExpApproximation.is_time_bounded appr) scc
    |> Set.to_array
    |> Parmap.array_parmap (Plrf.find_scc ~refined program is_exptime_bounded unbounded_vars scc)
    |> Array.to_sequence |> Sequence.filter_opt
  in
  (if compute_refined_plrfs then
     Sequence.of_list [ false; true ]
   else
     Sequence.of_list [ false ])
  |> Sequence.map ~f:find_plrfs
  |> Sequence.filter ~f:(not % Sequence.is_empty)
  |> fun seq ->
  Sequence.hd seq |? Sequence.empty
  |> MaybeChanged.fold_sequence ~f:(fun appr -> improve_with_plrf program (class_appr, appr)) ~init:appr
