(** Updates size and time-bounds until a fixed point is reached. *)
open Batteries
open Polynomials
open LocalSizeBound

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Analysis = Analysis.Make(PM)
  module Approximation = Approximation.MakeForClassicalAnalysis(PM)
  module CostBounds = CostBounds.Make(PM)
  module RVG = RVGTypes.MakeRVG(PM)
  module TrivialTimeBounds = TrivialTimeBounds.Make(PM)

  let find_bounds ~(preprocess: Program.t -> Program.t) ~conf ?(time_cfr = 180) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
    let rvg_with_sccs = RVG.rvg_with_sccs program in
    PartialEvaluation.time_cfr := float_of_int time_cfr;
    let (program_cfr,updated_appr) = appr
    |> TrivialTimeBounds.compute program
    |> Analysis.improve rvg_with_sccs ~conf ~preprocess `Time program in
    let appr_cost =
    updated_appr
    |> (fun appr ->
      if program_cfr |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_const (Transition.cost t))) then
        Tuple2.second @@ Analysis.improve ~conf rvg_with_sccs ~preprocess `Cost program_cfr appr
      else
        appr
    )
    |> CostBounds.infer_from_timebounds program_cfr in
    (program_cfr,appr_cost)
end

include Make(ProgramModules)
