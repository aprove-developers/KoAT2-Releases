open Batteries
open Program.Types
   
let compute program appr =
  let add_costbound transition appr =
    Approximation.add_costbound Bound.(of_poly (Transition.cost transition) * Approximation.timebound appr transition) transition appr
  in
  TransitionGraph.fold_edges_e add_costbound (Program.graph program) appr
