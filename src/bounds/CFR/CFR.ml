open Batteries

module CFR (Bound : BoundType.Bound) = struct
  open ProgramModules
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (ProgramModules)
  module TrivialTimeBounds = TrivialTimeBounds.Make (Bound) (ProgramModules)

  type approximation = Approximation.t

  let merge_appr (program : Program.t) (program_cfr : Program.t) appr =
    let unchanged_trans = Base.Set.inter (Program.transitions program) (Program.transitions program_cfr) in
    let appr_cfr = Approximation.empty |> TrivialTimeBounds.compute program_cfr in
    unchanged_trans
    |> Base.Set.fold
         ~f:(fun appr_cfr trans ->
           let timebound = Approximation.timebound appr trans
           and costbound = Approximation.costbound appr trans in
           appr_cfr
           |> Approximation.add_timebound timebound trans
           |> Approximation.add_costbound costbound trans)
         ~init:appr_cfr


  let lift_to_program transform program =
    MaybeChanged.(
      transform (Program.graph program) >>= fun graph -> same (Program.map_graph (fun _ -> graph) program))
end
