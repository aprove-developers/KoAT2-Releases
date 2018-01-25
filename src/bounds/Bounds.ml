open Batteries
open ProgramTypes
open Polynomials
   
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
  |> (fun appr ->
    if program |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_one (Transition.cost t))) then
      RankingBounds.improve `Cost program appr
    else
      MaybeChanged.same appr
  )
  |> MaybeChanged.unpack
  |> CostBounds.infer_from_timebounds program
