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

let improve program ?(scc = None) applied_cfr appr =
  let execute () =
    let module C = Graph.Components.Make(RVG) in
    [`Lower; `Upper]
    |> List.fold_left (fun appr kind ->
           let rvg = RVG.rvg kind program  in
           List.fold_left (fun appr rvg_scc -> 
              let current_time = Unix.time() in
              improve_scc kind program rvg appr rvg_scc
              |> tap (fun _ -> 
                      CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.time() -. current_time);
                      CFR.poll_timeout ~applied_cfr:applied_cfr)
            ) appr ( 
            if not (Option.is_some scc) then
             (List.rev (C.scc_list rvg))
            else ( 
             List.rev (List.filter (fun rvg_scc -> 
                (List.exists (fun (t,x) -> TransitionSet.mem t (Option.get scc)) rvg_scc)
               || ((List.length rvg_scc) == 1)) (C.scc_list rvg))
             )
    )) appr
  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute