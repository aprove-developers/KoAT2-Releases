open Batteries
open ProgramTypes
open RVGTypes
open BoundsInst

let logger = Logging.(get ExpSize)

module ERV = Make_RV(RVTransitions.TransitionForExpectedSize)

let improve_scc program ervg appr = function
  | [((gt,l),v)] when not (ERVG.mem_edge ervg ((gt,l),v) ((gt,l),v)) ->
     let new_bound = ExpTrivialSizeBounds.compute program
                      (fun (t,v) -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
                      (Approximation.expsizebound_abs appr)
                      (Approximation.timebound appr) ((gt,l),v) in

     Approximation.add_expsizebound new_bound (gt,l) v appr

  | scc ->
     let module TrExpSize = Set.Make2 (GeneralTransition) (Location) in
     let scc_vars = scc |> List.map ERV.variable |> VarSet.of_list in
     let trexpsize = scc |> List.map (fun ((gt,l),_) -> (gt,l)) |> TrExpSize.Product.of_list in
     let new_bound var = ExpNontrivialSizeBounds.compute program ervg (Approximation.timebound appr) (Approximation.exptimebound appr)
                                                                      (fun t v -> Bound.abs_bound @@ fun kind -> Approximation.sizebound kind appr t v)
                                                                      (Approximation.expsizebound_abs appr) scc var in
     (* Add all corresponding expected size bounds *)
     TrExpSize.Product.fold
      (fun (gt,l) appr' ->
        VarSet.fold (fun v appr'' -> Approximation.add_expsizebound (new_bound v) (gt,l) v appr'') scc_vars appr')
      trexpsize appr

let improve program appr =
  let execute () =
    let module C = Graph.Components.Make(ERVG) in
    let rvg = ERVG.rvg program in
    List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (List.rev (C.scc_list rvg))

  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute
