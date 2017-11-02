open Batteries
open Polynomials
open Program.Types
   
let logger = Logger.make_log "time"

(** All transitions outside of the prf transitions that lead to the given location. *)
let transitions_to (graph: TransitionGraph.t) (prf_transitions: Transition.t list) (location: Location.t): Transition.t Enum.t =
  let execute () =
    TransitionGraph.pred_e graph location
    |> TransitionSet.of_list
    |> fun transitions -> TransitionSet.(diff transitions (of_list prf_transitions))
    |> TransitionSet.enum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transitions to", ["location", Location.to_string location;
                                                   "prf_transitions", String.concat "," (List.map Transition.to_id_string prf_transitions)])
                     execute

(** All entry locations of the given transitions.
    Those are such locations, that are the target of any transition outside of the given transitions. *)
let entry_locations (graph: TransitionGraph.t) (prf_transitions: Transition.t list): Location.t Enum.t =
  let execute () =
    prf_transitions
    |> List.enum
    |> Enum.map Transition.src
    |> Enum.uniq_by Location.equal
    |> Enum.filter (fun location -> not Enum.(is_empty (transitions_to graph prf_transitions location)))
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "entry locations", ["prf_transitions", String.concat "," (List.map Transition.to_id_string prf_transitions)])
                     execute

let apply (appr: Approximation.t) (rank: Polynomial.t) (transition: Transition.t): Bound.t =
  let execute () =
    let (pol_plus, pol_minus) =
      Polynomial.separate_by_sign rank
      |> Tuple2.mapn Bound.of_poly
    in
    let insert_sizebounds kind bound =
      Bound.substitute_f (Approximation.sizebound kind appr transition) bound
    in
    Bound.(insert_sizebounds `Upper pol_plus - insert_sizebounds `Lower pol_minus)
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "apply to prf", ["rank", Polynomial.to_string rank;
                                                 "transition", Transition.to_id_string transition])
                     ~result:Bound.to_string
                     execute
  
let compute_timebound (appr: Approximation.t) (graph: TransitionGraph.t) (prf: RankingFunction.t) (transition: Transition.t): Bound.t =
  let execute () =
    entry_locations graph (RankingFunction.transitions prf)
    |> Enum.map (fun location ->
           transitions_to graph (RankingFunction.transitions prf) location
           |> Enum.map (fun transition -> (location,transition)) 
         )
    |> Enum.flatten
    |> Enum.map (fun (location,transition) -> Bound.(Approximation.timebound appr transition * max zero (apply appr (RankingFunction.rank prf location) transition)))
    |> Enum.fold Bound.add Bound.zero
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute time bound", ["transition", Transition.to_id_string transition])
                     ~result:Bound.to_string
                     execute
    
let improve program appr =
  let execute () =
    let prf = RankingFunction.find program appr in
    let strictly_decreasing = List.enum (RankingFunction.strictly_decreasing prf) in
    if Enum.count strictly_decreasing = 0 then
      MaybeChanged.same appr
    else
      strictly_decreasing
      |> Enum.fold (fun appr t -> Approximation.add_timebound (compute_timebound appr (Program.graph program) prf t) t appr) appr
      |> MaybeChanged.changed
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve time bounds", [])
                     execute
