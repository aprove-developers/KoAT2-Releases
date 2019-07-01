open Batteries
open ProgramTypes
open Polynomials
   
let rec find_bounds_ ?(degree = 5) ?(mrf = false) (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> SizeBounds.improve program
  |> RankingBounds.improve ~degree:degree ~mrf:mrf `Time program
  |> MaybeChanged.if_changed (find_bounds_ ~degree:degree ~mrf:mrf program)
  |> MaybeChanged.unpack

let find_bounds ?(degree = 5) ?(mrf = false) (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> TrivialTimeBounds.compute program
  |> find_bounds_ ~degree:degree ~mrf:mrf program
  |> (fun appr ->
    if program |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_one (Transition.cost t))) then
      RankingBounds.improve `Cost program appr
    else
      MaybeChanged.same appr
  )
  |> MaybeChanged.unpack
  |> CostBounds.infer_from_timebounds program
