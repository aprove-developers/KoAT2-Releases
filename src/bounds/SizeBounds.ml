open Batteries
open ProgramTypes
open RVGTypes
open CFR
   
let logger = Logging.(get Size)

let improve_scc kind program rvg appr = function
  | [((l,t,l'),v)] when not (RVG.mem_edge rvg ((l,t,l'),v) ((l,t,l'),v)) ->
     let new_bound = TrivialSizeBounds.compute kind program (fun kind -> Approximation.sizebound kind appr) ((l,t,l'),v) in
     Approximation.add_sizebound kind new_bound (l,t,l') v appr
  | scc ->
     let new_bound = NontrivialSizeBounds.compute kind program rvg (Approximation.timebound appr) (fun kind -> Approximation.sizebound kind appr) scc in
     Approximation.add_sizebounds kind new_bound scc appr
         
let improve program applied_cfr appr =
  let execute () =
    let module C = Graph.Components.Make(RVG) in
    [`Lower; `Upper]
    |> List.fold_left (fun appr kind ->
           let rvg = RVG.rvg kind program in
           List.fold_left (fun appr scc -> 
                let current_time = Unix.time() in
                improve_scc kind program rvg appr scc
                |> tap (fun _ -> 
                        CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.time() -. current_time);
                        if applied_cfr && !time_current_cfr < !delta_current_cfr then 
                          raise CFR.TIMEOUT)
            ) appr (List.rev (C.scc_list rvg))
         ) appr
  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute
