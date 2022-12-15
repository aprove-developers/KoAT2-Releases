open Batteries
open BoundsInst
open ProbabilisticProgramModules
open Approximation.Probabilistic

let logger = Logging.(get ExpTime)

let improve_with_plrf program (class_appr,appr) rank =
  let non_inc = Plrf.non_increasing rank in

  let incoming_gts = BoundsHelper.entry_gts program non_inc in
  let entry_locations = BoundsHelper.entry_locations_of_gts program non_inc incoming_gts in

  let new_bound =
    LocationSet.enum entry_locations
    |> Enum.map (fun entry_loc ->
        let entry_gts_to_loc =
          GeneralTransitionSet.filter (LocationSet.mem entry_loc % GeneralTransition.targets) incoming_gts
        in
        let class_trans_to_loc =
          GeneralTransitionSet.to_list entry_gts_to_loc
          |> List.map (fun gt ->
              GeneralTransition.transitions gt
              |> TransitionSet.filter (Location.equal entry_loc % Transition.target)
              |> TransitionSet.to_list
            )
          |> List.flatten
        in

        let rank_size_bound v =
          GeneralTransitionSet.enum entry_gts_to_loc
          |> Enum.map (fun gt -> ExpApproximation.sizebound appr (gt,entry_loc) v)
          |> RealBound.sum
        in
        let rank_at_loc = Plrf.rank rank entry_loc in
        let rank_bounded = RealBound.substitute_f rank_size_bound (RealBound.of_poly rank_at_loc) in

        let inc_det_timebound =
          List.enum class_trans_to_loc
          |> Enum.map (ClassicalApproximation.timebound class_appr)
          |> RealBound.of_intbound % Bound.sum
        in
        RealBound.(inc_det_timebound * rank_bounded)
      )
    |> RealBound.sum
  in
  Logger.log logger Logger.DEBUG (fun () -> "improve_with_plrf", [ "rank", Plrf.to_string rank
                                                                ; "bound", RealBound.to_string new_bound]) ;

  if RealBound.is_finite new_bound then
    (* TODO add to proof *)
    MaybeChanged.changed (ExpApproximation.add_timebound new_bound (Plrf.decreasing rank) appr)
  else
    MaybeChanged.same appr
