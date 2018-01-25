open Batteries
open ProgramTypes
   
let unbounded appr transition =
  Bound.is_infinity (Approximation.costbound appr transition)

let infer_from_timebounds program appr =
  let add_costbound transition appr =
    if unbounded appr transition then
      Approximation.add_costbound Bound.(of_poly (Transition.cost transition) * Approximation.timebound appr transition) transition appr
    else
      appr
  in
  TransitionGraph.fold_edges_e add_costbound (Program.graph program) appr
