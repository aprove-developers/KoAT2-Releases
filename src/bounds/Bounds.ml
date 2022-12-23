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
    let (program, appr) =
      TrivialTimeBounds.compute program appr
      |> Analysis.improve rvg_with_sccs ~conf ~preprocess program
    in
    program, appr
end

include Make(ProgramModules)
