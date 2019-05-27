open Batteries
open BoundsInst
open Polynomials
open ProgramTypes
open Formulas
open ExpBoundsHelper

let logger = Logging.(get ExpTime)

let get_best_bound program incoming_enum appr rankfunc : RealBound.t =
  let entry_locations =
    incoming_enum
    |> Enum.clone
    |> Enum.map snd
    |> Enum.uniq_by Location.equal
    |> List.of_enum
  in
  let incoming_list = List.of_enum incoming_enum in
  entry_locations
  |> Util.show_debug_log logger ~resultprint:(Util.enum_to_string Location.to_string % List.enum) "entry_locations"
  |> List.map (fun entry_location ->
       let entry_trans_to_location =
         incoming_list
         |> List.filter (Location.equal entry_location % snd)
         |> List.map fst
       in
       let trans_to_entry_location =
         entry_trans_to_location
         |> List.map GeneralTransition.transitions
         |> List.map @@ TransitionSet.filter (fun (_,t,l') -> Location.equal l' entry_location)
         |> List.map (TransitionSet.to_list)
         |> List.flatten
       in

       let rank_size_subst_bound v =
         entry_trans_to_location
         |> List.map (fun gt -> Approximation.expsizebound_abs appr (gt,entry_location) v )
         |> List.enum
         |> RealBound.sum
       in

       let det_timebound = List.map (Approximation.timebound appr) trans_to_entry_location |> List.enum |> Bound.sum |> RealBound.of_intbound in

       let rank = LexRSM.rank rankfunc entry_location in

       let rank_bounded = (RealBound.appr_substition_abs_all rank_size_subst_bound (RealBound.of_poly rank)) in

       let mul_inctime_and_rhs (inctime, rhs) = RealBound.(
         if is_infinity inctime then
           if equal zero rhs then
             zero
           else
             infinity
         else
           if is_infinity rhs then
             infinity
           else
             inctime * rhs
       )
       in
       mul_inctime_and_rhs (det_timebound,rank_bounded)
     )
  |> List.enum
  |> RealBound.sum

let compute_bound (appr: Approximation.t) (program: Program.t) (rank: LexRSM.t): RealBound.t =
  let execute () =
    rank
    |> LexRSM.non_increasing
    |> GeneralTransitionSet.to_list
    |> entry_transitions logger program
    |> fun incoming_list -> get_best_bound program incoming_list appr rank
  in Logger.with_log logger Logger.DEBUG
       (fun () -> "compute_bound", ["rank", LexRSM.pprf_to_string rank])
                     ~result:RealBound.to_string
                     execute

let improve_with_rank program appr (rank: LexRSM.t) =
  let bound = compute_bound appr program rank in
  if RealBound.is_infinity bound then
    MaybeChanged.same appr
  else
    rank
    |> LexRSM.decreasing
    |> (fun t -> Approximation.add_exptimebound bound t appr)
    |> MaybeChanged.changed

(** Checks if a transition is bounded *)
let exp_bounded appr transition =
  Approximation.is_exptime_bounded appr transition

let improve program appr =
  program
  |> Program.non_trivial_transitions
  |> GeneralTransitionSet.of_transitionset
  |> GeneralTransitionSet.filter (not % exp_bounded appr)
  |> GeneralTransitionSet.enum
  |> MaybeChanged.fold_enum (fun appr gt ->
         LexRSM.find program gt
         |> Option.map_default (fun rank ->
              improve_with_rank program appr rank
            ) (MaybeChanged.return appr)
       ) appr