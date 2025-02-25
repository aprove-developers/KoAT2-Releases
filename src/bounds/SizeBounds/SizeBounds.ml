open Batteries

let logger = Logging.(get Size)

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open! PM
  module RVG = RVGTypes.MakeRVG (PM)
  module Approximation = Approximation.MakeForClassicalAnalysis (Bounds.Bound) (PM)
  module NontrivialSizeBounds = NontrivialSizeBounds.Make (PM)
  module TrivialSizeBounds = TrivialSizeBounds.Make (PM)
  module LSB = LocalSizeBound.Make (PM.TransitionLabel) (PM.Transition) (PM.RV) (PM.Program)

  let improve_scc program rvg (get_lsb : PM.RV.modifier * Var.t -> (LSB.t_rec * bool Lazy.t) Option.t) appr =
    function
    | [ (m, v) ] when not (RVG.mem_edge rvg (m, v) (m, v)) ->
        let lsb_as_bound = get_lsb (m, v) |> Option.map (LSB.as_poly % Tuple2.first) in
        let new_bound =
          TrivialSizeBounds.compute program (Approximation.sizebound appr) (m, v) lsb_as_bound
        in
        Approximation.add_sizebound new_bound m v appr
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
