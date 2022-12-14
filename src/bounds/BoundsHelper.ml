open Batteries
open ProbabilisticProgramModules

(* TODO unify both. Should be possible if we always keep the target location *)
let entry_gts_with_locs program of_gts =
  GeneralTransitionSet.enum of_gts
  |> Enum.map (fun gt ->
      let loc = GeneralTransition.src gt in
      GeneralTransitionSet.enum (Program.pre_gt_cached program gt)
      |> Enum.filter (fun gt -> not (GeneralTransitionSet.mem gt of_gts))
      |> Enum.map (fun gt -> gt, loc)
    )
  |> Enum.flatten

let entry_gts program of_gts =
  Enum.map Tuple2.first (entry_gts_with_locs program of_gts)
  |> GeneralTransitionSet.of_enum

let entry_locations_of_gts program of_gts entry_gts =
  GeneralTransitionSet.enum of_gts
  |> Enum.map GeneralTransition.src
  |> LocationSet.of_enum
  |> LocationSet.filter (fun l ->
      GeneralTransitionSet.exists (LocationSet.mem l % GeneralTransition.targets) entry_gts
     )
