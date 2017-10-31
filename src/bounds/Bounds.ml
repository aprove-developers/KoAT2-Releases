open Batteries

let step (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> TimeBounds.improve program
  |> SizeBounds.improve program

let stop (program: Program.t) (appr: Approximation.t): bool =
  program
  |> Program.graph
  |> Program.TransitionGraph.transitions
  |> Program.TransitionSet.to_list
  |> Approximation.all_times_bounded appr

let rec find_bounds (program: Program.t) (appr: Approximation.t): Approximation.t =
  if stop program appr then
    appr
  else
    find_bounds program (step program appr)
