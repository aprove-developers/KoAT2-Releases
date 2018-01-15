open Batteries
open Program.Types
   
let infer_from_timebounds program appr =
  let add_costbound transition appr =
    Approximation.add_costbound Bound.(of_poly (Transition.cost transition) * Approximation.timebound appr transition) transition appr
  in
  TransitionGraph.fold_edges_e add_costbound (Program.graph program) appr

let infer_from_ranking program appr =
  RankingFunction.find_ `Cost program appr
  |> Option.map (fun rank ->
         rank
         |> RankingFunction.strictly_decreasing
         |> List.fold_left (fun appr t ->                
                Approximation.add_costbound (Bound.of_poly (RankingFunction.rank rank (Program.start program))) t appr
              ) appr
       )
  |? appr
  
let compute program appr =
  appr
  |> infer_from_timebounds program
  |> infer_from_ranking program
