open Batteries
open Polynomials
open ProgramTypes
   
let logger = Logging.(get Time)

type measure = [ `Cost | `Time ] [@@deriving show, eq]

(** All transitions outside of the prf transitions that lead to the given location. *)
let transitions_to (graph: TransitionGraph.t) (rank_transitions: Transition.t list) (location: Location.t): Transition.t List.t =
  TransitionGraph.pred_e graph location
  |> TransitionSet.of_list
  |> (fun transitions -> TransitionSet.diff transitions (TransitionSet.of_list rank_transitions))
  |> TransitionSet.to_list
  |> tap (fun transitions ->
         Logger.log logger Logger.DEBUG
           (fun () -> "transitions_to", ["location", Location.to_string location;
                                         "transitions", Util.enum_to_string Transition.to_id_string (List.enum transitions)]))

(** All entry locations of the given transitions.
    Those are such locations, that are the target of any transition outside of the given transitions. *)
let entry_locations (graph: TransitionGraph.t) (rank_transitions: Transition.t list): Location.t List.t =
  let execute () =
    rank_transitions
    |> List.enum
    |> Enum.map Transition.src
    |> Enum.uniq_by Location.equal
    |> Enum.filter (fun location -> not List.(is_empty (transitions_to graph rank_transitions location)))
    |> List.of_enum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "entry_locations", [])
                     ~result:(Util.enum_to_string Location.to_string % List.enum)
                     execute

let apply (get_sizebound: [`Lower | `Upper] -> Transition.t -> Var.t -> Bound.t) (rank: Polynomial.t) (transition: Transition.t): Bound.t =
  rank
  |> Bound.of_poly
  |> Bound.appr_substitution
       `Upper
       ~lower:(get_sizebound `Lower transition)
       ~higher:(get_sizebound `Upper transition)
  
let compute_bound (appr: Approximation.t) (graph: TransitionGraph.t) (rank: RankingFunction.t): Bound.t =
  let execute () =
    entry_locations graph (RankingFunction.non_increasing rank)
    |> List.enum
    |> Enum.map (fun location ->
           transitions_to graph (RankingFunction.non_increasing rank) location
           |> List.enum
           |> Enum.map (fun transition -> (location,transition)) 
         )
    |> Enum.flatten
    |> Enum.map (fun (location,transition) ->
           let timebound = Approximation.timebound appr transition in
           let rhs = Bound.(max zero (apply (fun kind -> Approximation.sizebound kind appr) (RankingFunction.rank rank location) transition)) in
           Bound.(
             if is_infinity timebound then
               if equal zero rhs then
                 zero
               else
                 infinity
             else
               if is_infinity rhs then
                 infinity
               else
                 timebound * rhs
           ))
    |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
       (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (RankingFunction.decreasing rank);
                                    "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (RankingFunction.non_increasing rank));
                                    "rank", RankingFunction.only_rank_to_string rank])
                     ~result:Bound.to_string
                     execute

let add_bound = function
  | `Time -> Approximation.add_timebound
  | `Cost -> Approximation.add_costbound
   
let improve_with_rank measure program appr rank =
  let execute () =
    let bound = compute_bound appr (Program.graph program) rank in
    if Bound.is_infinity bound then
      MaybeChanged.same appr
    else
      rank
      |> RankingFunction.decreasing
      |> (fun t -> add_bound measure bound t appr)
      |> MaybeChanged.changed
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve_with_rank", [])
                     execute

(** Checks if a transition is bounded *)
let bounded measure appr transition =
  match measure with
  | `Time -> Approximation.is_time_bounded appr transition
  | `Cost -> false
  
let improve measure program appr =
  let execute () =
    program
    |> Program.non_trivial_transitions
    |> TransitionSet.filter (fun t -> not (bounded measure appr t))
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum (fun appr transition ->
           RankingFunction.find measure program transition
           |> List.enum
           |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank
                ) appr           
         ) appr
  in Logger.with_log logger Logger.INFO
                     (fun () -> "improve_bounds", ["measure", show_measure measure])
                     execute
