open Batteries
open Polynomials
open ProgramTypes
   
let logger = Logging.(get Time)

type measure = [ `Cost | `Time ] [@@deriving show, eq]

(** All entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
let entry_transitions (program: Program.t) (rank_transitions: Transition.t list): Transition.t List.t =
  rank_transitions
  |> List.enum
  |> Enum.map (Program.pre program)
  |> Enum.flatten
  |> Enum.filter (fun r ->
         rank_transitions
         |> List.enum
         |> Enum.for_all (not % Transition.same r)
       )
  |> Enum.uniq_by Transition.same
  |> List.of_enum
  |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                               (fun () -> "entry_transitions", ["result", transitions |> List.enum |> Util.enum_to_string Transition.to_id_string]))

let apply (get_sizebound: [`Lower | `Upper] -> Transition.t -> Var.t -> Bound.t) (rank: Polynomial.t) (transition: Transition.t): Bound.t =
  rank
  |> Bound.of_poly
  |> Bound.appr_substitution
       `Upper
       ~lower:(get_sizebound `Lower transition)
       ~higher:(get_sizebound `Upper transition)
  
let compute_bound (appr: Approximation.t) (program: Program.t) (rank: RankingFunction.t): Bound.t =
  let execute () =
    rank
    |> RankingFunction.non_increasing
    |> entry_transitions program
    |> List.enum
    |> Enum.map (fun (l,t,l') ->
           let timebound = Approximation.timebound appr (l,t,l') in
           let rhs = Bound.(max zero (apply (fun kind -> Approximation.sizebound kind appr) (RankingFunction.rank rank l') (l,t,l'))) in
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
  let bound = compute_bound appr program rank in
  if Bound.is_infinity bound then
    MaybeChanged.same appr
  else
    rank
    |> RankingFunction.decreasing
    |> (fun t -> add_bound measure bound t appr)
    |> MaybeChanged.changed

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
