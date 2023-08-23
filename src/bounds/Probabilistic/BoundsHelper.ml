open! OurBase
open ProbabilisticProgramModules

(* TODO unify both. Should be possible if we always keep the target location *)
let entry_gts_with_locs program of_gts =
  Set.to_sequence of_gts
  |> Sequence.map ~f:(fun gt ->
         let loc = GeneralTransition.src gt in
         Set.to_sequence (Program.pre_gt program gt)
         |> Sequence.filter ~f:(fun gt -> not (Set.mem of_gts gt))
         |> Sequence.map ~f:(fun gt -> (gt, loc)))
  |> Sequence.join


let entry_gts program of_gts =
  Sequence.map ~f:Tuple2.first (entry_gts_with_locs program of_gts)
  |> Set.of_sequence (module GeneralTransition)


let entry_locations program of_gts =
  entry_gts_with_locs program of_gts |> Sequence.map ~f:Tuple2.second |> LocationSet.of_sequence
