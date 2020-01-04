(** Updates size and time-bounds until a fixed point is reached. *)
open Batteries
open ProgramTypes
open Polynomials

(** Updates size and time-bounds until a fixed point is reached and uses, if necessary, MRFs or CFR. *)
let rec find_bounds_ ?(mrf = false) ?(cfr = false) (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> SizeBounds.improve program
  |> RankingBounds.improve  ~mrf:mrf ~cfr:cfr `Time program 
  |> MaybeChanged.if_changed (find_bounds_  ~mrf:mrf program)
  |> MaybeChanged.unpack

(** Triggers size and time-bounds computation and, if necessary, sets maximal depth of MRFs. *)
let find_bounds ?(depth = 5) ?(mrf = false) ?(cfr = false) (program: Program.t) (appr: Approximation.t): Approximation.t =
  if mrf then( 
    MultiphaseRankingFunction.maxDepth := depth;
    MultiphaseRankingFunction.list_init depth);
  appr
  |> TrivialTimeBounds.compute program
  |> find_bounds_ ~mrf:mrf ~cfr:cfr program
  |> (fun appr ->
    if program |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_one (Transition.cost t))) then
      RankingBounds.improve `Cost program appr
    else
      MaybeChanged.same appr
  )
  |> MaybeChanged.unpack
  |> CostBounds.infer_from_timebounds program