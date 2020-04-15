open Batteries
open BoundsInst
open Polynomials
open ProgramTypes
open Formulas
open ExpBoundsHelper

let logger = Logging.(get ExpTime)

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

let get_best_bound entry_locations incoming_enum appr rankfunc: RealBound.t =
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

       mul_inctime_and_rhs (det_timebound,rank_bounded)
     )
  |> List.enum
  |> RealBound.sum

let compute_bounds ~refined (appr: Approximation.t) (program: Program.t) (rank: LexRSM.t): RealBound.t * RealBound.t =
  let execute () =
    let incoming_enum =
      rank |> LexRSM.non_increasing |> GeneralTransitionSet.to_list |> entry_transitions logger program
    in
    let entry_locations =
      incoming_enum
      |> Enum.clone
      |> Enum.map snd
      |> Enum.uniq_by Location.equal
      |> List.of_enum
    in

    let time =
      get_best_bound entry_locations (Enum.clone incoming_enum) appr rank
      |> RealBound.simplify_vars_nonnegative
    in

    let entry_ts_to_decreasing =
      GeneralTransition.transitions (LexRSM.decreasing rank)
      |> TransitionSet.enum
      |> Enum.map (Program.pre program)
      |> Enum.flatten
    in

    let inc_det_sizebound v =
      entry_ts_to_decreasing |> Enum.clone
      |> Enum.map (fun t -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
      |> Bound.maximum
      |> RealBound.of_intbound
    in

    let substituted_cost =
      GeneralTransition.cost (LexRSM.decreasing rank)
      |> RealBound.appr_substition_abs_all inc_det_sizebound
    in

    let cost = mul_inctime_and_rhs (time,substituted_cost) |> RealBound.simplify_vars_nonnegative in

    (time, cost)

  in Logger.with_log logger Logger.DEBUG
       (fun () -> "compute_bound", ["rank", LexRSM.pprf_to_string rank ^ (if refined then " (refined)" else "")])
                     ~result:(fun (time,cost) -> "time: "^RealBound.to_string time ^ " cost: " ^ (RealBound.to_string cost))
                     execute

let improve_with_rank ~refined add_exptimebound add_expcostbound program appr (rank: LexRSM.t) =
  let (time,cost) = compute_bounds ~refined:refined appr program rank in
  (if RealBound.is_infinity time || Approximation.is_exptime_bounded appr (LexRSM.decreasing rank) then
      MaybeChanged.same appr
   else
     MaybeChanged.changed (add_exptimebound time (LexRSM.decreasing rank) appr)
  )
  |> (fun mca ->
        if Approximation.is_expcost_bounded appr (LexRSM.decreasing rank) || RealBound.is_infinity cost then
          mca
        else
          MaybeChanged.(mca >>= (changed % add_expcostbound cost (LexRSM.decreasing rank)))
     )

(** Checks if a transition is bounded *)
let exp_bounded appr transition =
  Approximation.is_expcost_bounded appr transition && Approximation.is_exptime_bounded appr transition

let improve ~refined ~refined_smt_timeout add_exptimebound add_expcostbound cache program appr =
  program
  |> Program.non_trivial_transitions
  |> GeneralTransitionSet.of_transitionset
  |> GeneralTransitionSet.filter (not % exp_bounded appr)
  |> GeneralTransitionSet.enum
  |> MaybeChanged.fold_enum (fun appr gt ->
         LexRSM.find ~refined:refined ~timeout:(if refined then refined_smt_timeout else None) cache program gt
         |> Option.map_default (fun rank ->
              improve_with_rank ~refined:refined add_exptimebound add_expcostbound program appr rank
            ) (MaybeChanged.return appr)
       ) appr
