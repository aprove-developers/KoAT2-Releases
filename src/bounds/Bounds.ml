(** Updates size and time-bounds until a fixed point is reached. *)
open Batteries
open ProgramTypes
open Polynomials
open LocalSizeBound

let find_bounds ?(mprf_max_depth = 1) ?(cfr = false) ?(fast = false) ?(time_cfr = 180) ?(inv = false) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
  let rvg = RVGTypes.RVG.rvg program in
   if cfr then
     CFR.number_unsolved_trans := (Program.cardinal_trans_scc program);
     CFR.time_cfr := float_of_int time_cfr;

  let (program_cfr,updated_appr) = appr
  |> TrivialTimeBounds.compute program
  |> RankingBounds.improve rvg ~mprf_max_depth ~cfr ~inv ~fast `Time program in
  let appr_cost =
  updated_appr
  |> (fun appr ->
    if program_cfr |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_const (Transition.cost t))) then
      Tuple2.second @@ RankingBounds.improve rvg ~mprf_max_depth ~cfr ~inv ~fast `Cost program_cfr appr
    else
      appr
  )
  |> CostBounds.infer_from_timebounds program_cfr in
  (program_cfr,appr_cost)
