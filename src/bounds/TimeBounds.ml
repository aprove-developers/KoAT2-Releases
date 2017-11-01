open Batteries
open Polynomials
   
(** All transitions outside of the prf transitions that lead to the given location. *)
let transitions_to (graph: Program.TransitionGraph.t) (prf_transitions: Program.Transition.t list) (location: Program.Location.t): Program.Transition.t Enum.t =
  Program.TransitionGraph.pred_e graph location
  |> Program.TransitionSet.of_list
  |> fun transitions -> Program.TransitionSet.(diff transitions (of_list prf_transitions))
  |> Program.TransitionSet.enum

(** All entry locations of the given transitions.
    Those are such locations, that are the target of any transition outside of the given transitions. *)
let entry_locations (graph: Program.TransitionGraph.t) (prf_transitions: Program.Transition.t list): Program.Location.t Enum.t =
  prf_transitions
  |> List.enum
  |> Enum.map Program.Transition.src
  |> Enum.uniq_by Program.Location.equal
  |> Enum.filter (fun location -> not Enum.(is_empty (transitions_to graph prf_transitions location)))

let apply (appr: Approximation.t) (polynomial: Polynomial.t) (transition: Program.Transition.t): Bound.t =
  let (pol_plus, pol_minus) =
    Polynomial.separate_by_sign polynomial
    |> Tuple2.mapn Bound.of_poly
  in
  let insert_sizebounds kind bound =
    Bound.substitute_f (Approximation.sizebound kind appr transition) bound
  in
  Bound.(insert_sizebounds `Upper pol_plus - insert_sizebounds `Lower pol_minus)
  
let compute_timebound (appr: Approximation.t) (graph: Program.TransitionGraph.t) (prf: RankingFunction.t) (transition: Program.Transition.t): Bound.t =
  entry_locations graph (RankingFunction.transitions prf)
  |> Enum.map (fun location ->
         transitions_to graph (RankingFunction.transitions prf) location
         |> Enum.map (fun transition -> (location,transition)) 
       )
  |> Enum.flatten
  |> Enum.map (fun (location,transition) -> Bound.(Approximation.timebound appr transition * max zero (apply appr (RankingFunction.rank prf location) transition)))
  |> Enum.fold Bound.add Bound.zero
    
let improve program appr =
  let prf = RankingFunction.find program appr in
  let strictly_decreasing = List.enum (RankingFunction.strictly_decreasing prf) in
  if Enum.count strictly_decreasing = 0 then
    MaybeChanged.same appr
  else
    strictly_decreasing
    |> Enum.fold (fun appr t -> Approximation.add_timebound (compute_timebound appr (Program.graph program) prf t) t appr) appr
    |> MaybeChanged.changed
