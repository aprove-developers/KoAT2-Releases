open Batteries
open ProbabilisticProgramModules

let entry_gts program of_gts =
  let all_possible_entry_gts =
    GeneralTransitionSet.enum of_gts
    |> Enum.map (Program.pre_gt_cached program)
    |> Enum.flatten % Enum.map GeneralTransitionSet.enum
    |> GeneralTransitionSet.of_enum
  in
  GeneralTransitionSet.diff all_possible_entry_gts of_gts

let entry_locations_of_gts program of_gts entry_gts =
  GeneralTransitionSet.enum of_gts
  |> Enum.map GeneralTransition.src
  |> LocationSet.of_enum
  |> LocationSet.filter (fun l ->
      GeneralTransitionSet.exists (Location.equal l % GeneralTransition.src) entry_gts
     )
