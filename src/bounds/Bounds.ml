open Batteries

let rec find_bounds_ (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> SizeBounds.improve program
  |> RankingBounds.improve `Time program
  |> MaybeChanged.if_changed (find_bounds_ program)
  |> MaybeChanged.unpack

let find_bounds (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> TrivialTimeBounds.compute program
  |> find_bounds_ program
  |> RankingBounds.improve `Cost program
  |> MaybeChanged.unpack
  |> CostBounds.infer_from_timebounds program
