open Batteries
open ProgramTypes
open RVGTypes

let logger = Logging.(get Size)

module RV = Make_RV(Transition)

let improve_scc lsb_cache kind program rvg appr = function
  | [((l,t,l'),v)] when not (RVG.mem_edge rvg ((l,t,l'),v) ((l,t,l'),v)) ->
     let new_bound = TrivialSizeBounds.compute lsb_cache kind program (fun kind -> Approximation.sizebound kind appr) ((l,t,l'),v) in
     Approximation.add_sizebound kind new_bound (l,t,l') v appr
  | scc ->
     let new_bound = NontrivialSizeBounds.compute lsb_cache kind program rvg (Approximation.timebound appr) (fun kind -> Approximation.sizebound kind appr) scc in
     Approximation.add_sizebounds kind new_bound scc appr

let improve lsb_cache program appr =
  let execute () =
    let module C = Graph.Components.Make(RVG) in
    [`Lower; `Upper]
    |> List.fold_left (fun appr kind ->
           let rvg = RVG.rvg lsb_cache kind program in
           List.fold_left (fun appr scc -> improve_scc lsb_cache kind program rvg appr scc) appr (List.rev (C.scc_list rvg))
         ) appr
  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute
