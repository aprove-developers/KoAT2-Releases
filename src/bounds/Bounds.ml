(** Updates size and time-bounds until a fixed point is reached. *)
open Batteries
open ProgramTypes
open Polynomials
open LocalSizeBound

let find_bounds ?(mprf_max_depth = 1) ~preprocess ~local ?(cfr = false) ?(fast = false) ?(time_cfr = 180) ?(inv = false) ?(twn = false) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
  let rvg_with_sccs = RVGTypes.RVG.rvg_with_sccs program in
   if cfr then
    CFR.time_cfr := float_of_int time_cfr;
  let (program_cfr,updated_appr) = appr
  |> TrivialTimeBounds.compute program
  |> Analysis.improve rvg_with_sccs ~mprf_max_depth ~preprocess ~local ~cfr ~inv ~fast `Time program in
  let appr_cost =
  updated_appr
  |> (fun appr ->
    if program_cfr |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_const (Transition.cost t))) then
      Tuple2.second @@ Analysis.improve rvg_with_sccs ~mprf_max_depth ~preprocess ~local ~cfr ~inv ~fast `Cost program_cfr appr
    else
      appr
  )
  |> CostBounds.infer_from_timebounds program_cfr in
  (program_cfr,appr_cost)