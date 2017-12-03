open Batteries
open Program.Types
   
let logger = Logging.(get Size)

let improve_scc program rvg appr = function
  | [((l,t,l'),v)] when not (Location.equal l l') ->
     let add_trivial_bound kind =
       let new_bound = TrivialSizeBounds.compute kind program (fun kind -> Approximation.sizebound kind appr) ((l,t,l'),v) in
       Approximation.add_sizebound kind new_bound (l,t,l') v appr
     in appr
     |> fun appr -> add_trivial_bound `Upper
     |> fun appr -> add_trivial_bound `Lower
  | scc ->
     let add_nontrivial_bound kind =
       let new_bound = NontrivialSizeBounds.compute kind program rvg (Approximation.timebound appr) (fun kind -> Approximation.sizebound kind appr) scc in
       Approximation.add_sizebounds kind new_bound scc appr
     in appr
     |> fun appr -> add_nontrivial_bound `Upper
     |> fun appr -> add_nontrivial_bound `Lower
         
let improve program appr =
  let execute () =
    let module C = Graph.Components.Make(RVG) in
    let rvg = Program.rvg program in
    List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (List.rev (C.scc_list rvg))
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "improve size bounds", [])
                  execute
