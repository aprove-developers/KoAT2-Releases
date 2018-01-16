open Batteries
open Polynomials
open Program.Types
   
let logger = Logging.(get Time)

type measure = [ `Cost | `Time ] [@@deriving show, eq]

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
  
let compute_bound (appr: Approximation.t) (graph: TransitionGraph.t) (prf: RankingFunction.t): Bound.t =
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
                     (fun () -> "compute bound", ["prf", RankingFunction.to_string prf])
                     ~result:Bound.to_string
                     execute

let add_bound = function
  | `Time -> Approximation.add_timebound
  | `Cost -> Approximation.add_costbound
   
let improve_with_rank measure program appr maybe_rank =
  let execute () =
    maybe_rank
    |> Option.map (fun rank ->
           let bound = compute_bound appr (Program.graph program) rank in
           if Bound.is_infinity bound then
             MaybeChanged.same appr
           else
             rank
             |> RankingFunction.decreasing
             |> fun t -> add_bound measure bound t appr
             |> MaybeChanged.changed
         )
    |? MaybeChanged.same appr
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve bounds with rank", [])
                     execute

(** Checks if a transition is bounded *)
let bounded measure appr transition =
  match measure with
  | `Time -> Approximation.is_time_bounded appr transition
  | `Cost -> false
  
let improve measure program appr =
  let execute () =
    let module SCC = Graph.Components.Make(TransitionGraph) in
    program
    |> Program.graph
    |> SCC.scc_list
    |> List.rev
    |> List.enum
    |> Enum.map (TransitionGraph.loc_transitions (Program.graph program))
    |> Enum.filter (not % TransitionSet.is_empty)
    |> MaybeChanged.fold_enum (fun appr transitions ->
           transitions
           |> TransitionSet.filter (fun t -> not (bounded measure appr t))
           |> TransitionSet.enum
           |> MaybeChanged.fold_enum (fun appr transition ->
                  RankingFunction.find measure
                                       (Program.vars program)
                                       transitions
                                       transition
                  |> improve_with_rank measure program appr
                ) appr
         ) appr
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve bounds", ["measure", show_measure measure])
                     execute
