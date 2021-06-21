(** Updates size and time-bounds until a fixed point is reached. *)
open Batteries
open ProgramTypes
open Polynomials
open LocalSizeBound

let find_bounds ?(depth = 1) ?(cfr = false) ?(fast = false) ?(time_cfr = 180) ?(inv = false) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
  let rvg = RVGTypes.RVG.rvg program in
   if cfr then
     CFR.number_unsolved_trans := (Program.cardinal_trans_scc program);
     CFR.time_cfr := float_of_int time_cfr;

   MultiphaseRankingFunction.maxDepth := depth;
   let cache_mprf = MultiphaseRankingFunction.new_cache () in
  let (program_cfr,updated_appr,_) = appr
  |> TrivialTimeBounds.compute program
  |> RankingBounds.improve rvg cache_mprf ~cfr:cfr ~inv:inv ~fast:fast `Time program in
  let appr_cost =
  updated_appr
  |> (fun appr ->
    if program_cfr |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_const (Transition.cost t))) then
      RankingBounds.improve rvg cache_mprf ~cfr:false ~inv:inv ~fast:fast `Cost program_cfr appr
      |> Tuple3.second
    else
      appr
  )
  |> CostBounds.infer_from_timebounds program_cfr in
  (program_cfr,appr_cost)
