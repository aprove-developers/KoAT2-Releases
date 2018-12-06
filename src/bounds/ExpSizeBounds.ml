open Batteries
open ProgramTypes
open RVGTypes

let logger = Logging.(get ExpSize)

module ERV = Make_RV(RVTransitions.TransitionForExpectedSize)

let improve_scc kind program ervg appr = function
  | [((gt,l),v)] when not (ERVG.mem_edge ervg ((gt,l),v) ((gt,l),v)) ->
     let new_bound = ExpTrivialSizeBounds.compute kind program
                      (fun kind -> Approximation.sizebound kind appr)
                      (fun kind -> Approximation.expsizebound kind appr)
                      (Approximation.timebound appr) ((gt,l),v) in

     Approximation.add_expsizebound kind new_bound (gt,l) v appr

  | scc ->
     let module TrExpSize = Set.Make2 (GeneralTransition) (Location) in
     let scc_vars = scc |> List.map ERV.variable |> VarSet.of_list in
     let trexpsize = scc |> List.map (fun ((gt,l),_) -> (gt,l)) |> TrExpSize.Product.of_list in
     let new_bound var = ExpNontrivialSizeBounds.compute kind program ervg (Approximation.timebound appr) (Approximation.exptimebound appr)
                                                                           (fun kind -> Approximation.sizebound kind appr)
                                                                           (fun kind -> Approximation.expsizebound kind appr) scc var in
     (* Add all corresponding expected size bounds *)
     TrExpSize.Product.fold
      (fun (gt,l) appr' ->
        VarSet.fold (fun v appr'' -> Approximation.add_expsizebound kind (new_bound v) (gt,l) v appr'') scc_vars appr')
      trexpsize appr

let improve program appr =
  let execute () =
    let module C = Graph.Components.Make(ERVG) in
    [`Lower; `Upper]
    |> List.fold_left (fun appr kind ->
           let rvg = ERVG.rvg kind program in
           List.fold_left (fun appr scc -> improve_scc kind program rvg appr scc) appr (List.rev (C.scc_list rvg))
         ) appr
  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute
