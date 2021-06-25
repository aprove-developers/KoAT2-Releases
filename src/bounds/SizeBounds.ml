open Batteries
open ProgramTypes
open RVGTypes
open CFR

let logger = Logging.(get Size)

let improve_scc program rvg appr = function
  | [((l,t,l'),v)] when not (RVG.mem_edge rvg ((l,t,l'),v) ((l,t,l'),v)) ->
     let new_bound = TrivialSizeBounds.compute program rvg (Approximation.sizebound appr) ((l,t,l'),v) in
     Approximation.add_sizebound new_bound (l,t,l') v appr
  | scc ->
     let new_bound = NontrivialSizeBounds.compute program rvg (Approximation.timebound appr) (Approximation.sizebound appr) scc in
     Approximation.add_sizebounds new_bound scc appr

let improve program rvg ?(scc = None) appr =
  let execute () =
    let module C = Graph.Components.Make(RVG) in

    List.fold_left (fun appr rvg_scc -> improve_scc program rvg appr rvg_scc) appr (
      if not (Option.is_some scc) then
        (List.rev (C.scc_list rvg))
      else (
      List.rev (List.filter (fun rvg_scc -> (List.exists (fun (t,x) -> TransitionSet.mem t (Option.get scc)) rvg_scc)
                                            || ((List.length rvg_scc) == 1)) (C.scc_list rvg))
      )
    )
  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute
