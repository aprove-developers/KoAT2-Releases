open Batteries
open ProgramTypes
open RVGTypes
open BoundsInst

let logger = Logging.(get ExpSize)

module ERV = Make_RV(RVTransitions.TransitionForExpectedSize)

let improve_scc simplify_smt elsb_cache program ervg appr = function
  | [((gt,l),v)] when not (ERVG.mem_edge ervg ((gt,l),v) ((gt,l),v)) ->
     let new_bound = ExpTrivialSizeBounds.compute elsb_cache program
                      (fun (t,v) -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
                      (Approximation.expsizebound_abs appr)
                      (Approximation.timebound_gt appr) ((gt,l),v) in

     Approximation.add_expsizebound simplify_smt new_bound (gt,l) v appr

  | scc ->
     let new_bound = ExpNontrivialSizeBounds.compute elsb_cache program (Approximation.timebound_gt appr) (Approximation.exptimebound appr)
                                                                            (fun t v -> Bound.abs_bound @@ fun kind -> Approximation.sizebound kind appr t v)
                                                                            (Approximation.expsizebound_abs appr) scc in
     (* Add all new bound to all general result variables *)
     Approximation.add_expsizebounds simplify_smt new_bound scc appr

let improve simplify_smt elsb_cache ervg sccs program appr =
  let execute () =
    List.fold_left (fun appr scc -> improve_scc simplify_smt elsb_cache program ervg appr scc) appr sccs

  in Logger.with_log logger Logger.INFO
                  (fun () -> "improve_size_bounds", [])
                  execute
