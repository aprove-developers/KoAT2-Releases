open Batteries
open Polynomials
open Program.Types
   
let logger = Logging.(get Time)

(** All transitions outside of the prf transitions that lead to the given location. *)
let transitions_to (graph: TransitionGraph.t) (prf_transitions: Transition.t list) (location: Location.t): Transition.t List.t =
  let execute () =
    TransitionGraph.pred_e graph location
    |> TransitionSet.of_list
    |> fun transitions -> TransitionSet.(diff transitions (of_list prf_transitions))
    |> TransitionSet.to_list
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transitions to", ["location", Location.to_string location;
                                                   "T'", String.concat "," (List.map Transition.to_id_string prf_transitions)])
                     ~result:(fun transitions -> transitions |> List.map Transition.to_id_string |> String.concat ", ")
                     execute

(** All entry locations of the given transitions.
    Those are such locations, that are the target of any transition outside of the given transitions. *)
let entry_locations (graph: TransitionGraph.t) (prf_transitions: Transition.t list): Location.t List.t =
  let execute () =
    prf_transitions
    |> List.enum
    |> Enum.map Transition.src
    |> Enum.uniq_by Location.equal
    |> Enum.filter (fun location -> not List.(is_empty (transitions_to graph prf_transitions location)))
    |> List.of_enum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "entry locations", ["T'", String.concat "," (List.map Transition.to_id_string prf_transitions)])
                     ~result:(fun locations -> locations |> List.map Location.to_string |> String.concat ", ")
                     execute

let apply (get_sizebound: [`Lower | `Upper] -> Transition.t -> Var.t -> Bound.t) (rank: Polynomial.t) (transition: Transition.t): Bound.t =
  let execute () =
    rank
    |> Bound.of_poly
    |> Bound.appr_substitution
         `Upper
         ~lower:(get_sizebound `Lower transition)
         ~higher:(get_sizebound `Upper transition)
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "apply to prf", ["rank", Polynomial.to_string rank;
                                                 "transition", Transition.to_id_string transition])
                     ~result:Bound.to_string
                     execute
  
let compute_timebound (appr: Approximation.t) (graph: TransitionGraph.t) (prf: RankingFunction.t): Bound.t =
  let execute () =
    entry_locations graph (RankingFunction.transitions prf)
    |> List.enum
    |> Enum.map (fun location ->
           transitions_to graph (RankingFunction.transitions prf) location
           |> List.enum
           |> Enum.map (fun transition -> (location,transition)) 
         )
    |> Enum.flatten
    |> Enum.map (fun (location,transition) ->
           Bound.(Approximation.timebound appr transition *
                    max zero (apply (fun kind -> Approximation.sizebound kind appr) (RankingFunction.rank prf location) transition)))
    |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute time bound", ["prf", RankingFunction.to_string prf])
                     ~result:Bound.to_string
                     execute
    
let improve_with_pol program appr pol =
  let execute () =
    if Option.is_none pol then
      MaybeChanged.same appr
    else
      let timebound = compute_timebound appr (Program.graph program) (Option.get pol) in
      if Bound.is_infinity timebound then
        MaybeChanged.same appr
      else
        pol
        |> Option.get
        |> RankingFunction.strictly_decreasing
        |> List.enum
        |> Enum.fold (flip (Approximation.add_timebound timebound)) appr
        |> MaybeChanged.changed
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve time bounds with pol", [])
                     execute

(** Checks if a transition has already been oriented strictly in a given approximation *)
let unbound appr transition =  
  Bound.is_infinity (Approximation.timebound appr transition)
    
let improve program appr =
  let execute () =
    let module SCC = Graph.Components.Make(TransitionGraph) in
    program
    |> Program.graph
    |> SCC.scc_list
    |> List.rev
    |> List.enum
    |> Enum.map (TransitionGraph.loc_transitions (Program.graph program))
    |> Enum.filter (not % TransitionSet.is_empty)
    |> Enum.map TransitionSet.to_list
    |> MaybeChanged.fold_enum (fun appr transitions ->
           improve_with_pol program appr (RankingFunction.find `Time (Program.vars program) transitions appr)
         ) appr
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve time bounds", [])
                     execute
