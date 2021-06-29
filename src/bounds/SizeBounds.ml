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

let improve program (rvg, rvg_sccs) ?(scc = None) appr =
  let execute () =
    let considered_sccs =
      match scc with
        | None -> List.rev (Lazy.force rvg_sccs)
        | Some scc ->
            Lazy.force rvg_sccs
            |> List.filter (fun rvg_scc -> List.exists (fun (t,_) -> TransitionSet.mem t scc) rvg_scc || List.length rvg_scc == 1)
            |> List.rev
    in

    List.fold_left (improve_scc program rvg) appr considered_sccs

  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute
