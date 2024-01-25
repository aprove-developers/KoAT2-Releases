open Batteries

let logger = Logging.(get Size)

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open! PM
  module RVG = RVGTypes.MakeRVG (PM)
  module Approximation = Approximation.MakeForClassicalAnalysis (Bounds.Bound) (PM)
  module LSB = LocalSizeBound.Make (PM.TransitionLabel) (PM.Transition) (PM.Program)
  module NontrivialSizeBounds = NontrivialSizeBounds.Make (PM)
  module TrivialSizeBounds = TrivialSizeBounds.Make (PM)

  let improve_scc program rvg get_lsb appr = function
    | [ ((l, t, l'), v) ] when not (RVG.mem_edge rvg ((l, t, l'), v) ((l, t, l'), v)) ->
        let lsb_as_bound = get_lsb ((l, t, l'), v) |> Option.map (LSB.as_bound % Tuple2.first) in
        let new_bound =
          TrivialSizeBounds.compute program (Approximation.sizebound appr) ((l, t, l'), v) lsb_as_bound
        in
        Approximation.add_sizebound new_bound (l, t, l') v appr
    | scc ->
        let new_bound =
          NontrivialSizeBounds.compute program rvg (Approximation.timebound appr)
            (Approximation.sizebound appr) scc get_lsb
        in
        Approximation.add_sizebounds new_bound scc appr


  let improve program (rvg, rvg_sccs) get_lsb appr =
    let execute () =
      let rvg_sccs = List.rev (Lazy.force rvg_sccs) in
      List.fold_left (improve_scc program rvg get_lsb) appr rvg_sccs
    in

    Logger.with_log logger Logger.INFO (fun () -> ("improve_size_bounds", [])) execute
end

include Make (ProgramModules)
