open Batteries

let rec find_bounds (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> SizeBounds.improve program
  |> TimeBounds.improve program
  |> MaybeChanged.if_changed (SizeBounds.improve program)
  |> MaybeChanged.if_changed (find_bounds program)
  |> MaybeChanged.unpack
