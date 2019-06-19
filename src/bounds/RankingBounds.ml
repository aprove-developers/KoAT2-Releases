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


(* Compute new Timebounds for MRFs*)

let compute_eta_k (k:int)= List.init (k - 1) (fun i -> 1)

let rec sum list =
  match list with
  | [] -> 0
  | [x] -> x
  | x::xs -> x + sum xs

let rec sumProduct (list1, list2) =
  match (list1,list2) with
    | ([x],[y]) -> x * y
    | (x::xs,y::ys) -> x * y + sumProduct (xs, ys)
    | _ -> 0

let rec compute_coefficients (degree:int) list =
  match list with
  | [] -> [(1,1)]
  | _  ->
    (if degree == List.length list then list
     else
       let k = List.length list in
       let etas = compute_eta_k k in
       let cks = List.init (k - 1) (fun i -> fst (List.nth list i)) in
       let ck = (1 + sum etas) + (sumProduct (cks, etas)) * (k - 1) + (snd (List.nth list (k - 1))) in
       let dk = (List.nth etas k) * (int_of_float (ceil (float (snd (List.nth list (k - 1))) /. float k))) in
       compute_coefficients (degree:int) list@[(ck,dk)])

let rec maximum_coefficients list =
  match list with
  | [(x,y)] -> int_of_float(ceil (float x /. float y))
  | (x,y) :: rest -> max (int_of_float (ceil (float x /. float y))) (maximum_coefficients rest)
  | _ -> 0

let rec maxBound_of_list list =
 match list with
 | [] -> Bound.zero
 | [x] -> x
 | x::xs -> Bound.max x (maxBound_of_list xs)

let compute_bound_mrf (appr: Approximation.t) (program: Program.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> entry_transitions program
   |> List.enum
   |> Enum.map (fun (l,t,l') ->
       Printf.printf "hi \n";
       let timebound = Approximation.timebound appr (l,t,l') in
         let coefficients = (compute_coefficients (MultiphaseRankingFunction.degree rank) []) in
         let maximum_coefficient = (maximum_coefficients coefficients) in
         let evaluate = (fun rank -> (apply (fun kind -> Approximation.sizebound kind appr) rank) (l,t,l')) in
         let var = (List.init (MultiphaseRankingFunction.degree rank) (fun i -> (evaluate ((List.nth (MultiphaseRankingFunction.rank rank) i) l')))) in
         let rhs = Bound.(mul (Bound.of_int maximum_coefficient) (max zero (maxBound_of_list var))) in
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
      (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (MultiphaseRankingFunction.decreasing rank);
                                   "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (MultiphaseRankingFunction.non_increasing rank));
                                   "rank", MultiphaseRankingFunction.only_rank_to_string rank;])
                    ~result:Bound.to_string
                    execute

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

  let improve_with_rank_mrf measure program appr rank =
    let bound = compute_bound_mrf appr program rank in
    if Bound.is_infinity bound then
      MaybeChanged.same appr
    else
      rank
      |> MultiphaseRankingFunction.decreasing
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
    |> MaybeChanged.fold_enum (
      if not false then
      (fun appr transition ->
           MultiphaseRankingFunction.find measure program transition
           |> List.enum
           |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mrf measure program appr rank
             ) appr)
      else
      (fun appr transition ->
           RankingFunction.find measure program transition
           |> List.enum
           |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank
             ) appr)

         ) appr
  in Logger.with_log logger Logger.INFO
                     (fun () -> "improve_bounds", ["measure", show_measure measure])
                     execute
