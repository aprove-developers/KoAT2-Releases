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

(* The first returned bound is the bound obtained by substituting the ranking function with expected sizebounds.
   The second returned bound is the bound where the ranking functions variable are replaced with *non-deterministic* size bounds *)
let get_best_bound entry_locations incoming_enum appr rankfunc : RealBound.t * RealBound.t =
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

       let rank_size_subst_prob_bound v =
         entry_trans_to_location
         |> List.map (fun gt -> Approximation.expsizebound_abs appr (gt,entry_location) v )
         |> List.enum
         |> RealBound.sum
       in
       let rank_size_subst_nonprob_bound v =
         entry_trans_to_location
         |> List.enum
         |> Enum.map (TransitionSet.filter (Location.equal entry_location % Transition.target) % GeneralTransition.transitions)
         |> Enum.map TransitionSet.enum
         |> Enum.flatten
         |> TransitionSet.of_enum
         |> TransitionSet.enum
         |> Enum.map (fun t -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
         |> Bound.maximum
         |> RealBound.of_intbound
       in

       let det_timebound = List.map (Approximation.timebound appr) trans_to_entry_location |> List.enum |> Bound.sum |> RealBound.of_intbound in

       let rank = LexRSM.rank rankfunc entry_location in

       let rank_prob_bounded = (RealBound.appr_substition_abs_all rank_size_subst_prob_bound (RealBound.of_poly rank)) in

       let rank_nonprob_bounded = RealBound.appr_substition_abs_all rank_size_subst_nonprob_bound (RealBound.of_poly rank) in

       (mul_inctime_and_rhs (det_timebound,rank_prob_bounded), mul_inctime_and_rhs (det_timebound, rank_nonprob_bounded))
     )
  |> List.enum
  |> fun en -> (RealBound.sum @@ Enum.map Tuple2.first (Enum.clone en), RealBound.sum @@ Enum.map Tuple2.second (Enum.clone en))

let compute_bounds (appr: Approximation.t) (program: Program.t) (rank: LexRSM.t): RealBound.t * RealBound.t * RealBound.t =
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

    let (best_bound_prob_size, best_bound_nonprob_size) =
      get_best_bound entry_locations (Enum.clone incoming_enum) appr rank
    in
    let time = best_bound_prob_size |> RealBound.(max zero) |> RealBound.simplify_vars_nonnegative in

    let time_nonprob_size = best_bound_nonprob_size |> RealBound.(max zero) |> RealBound.simplify_vars_nonnegative in

    let entry_ts_to_decreasing =
      GeneralTransition.transitions (LexRSM.decreasing rank)
      |> TransitionSet.enum
      |> Enum.map (Program.pre program)
      |> Enum.flatten
      |> TransitionSet.of_enum
    in

    let entry_gts_to_decreasing =
      Program.pre_gt program (LexRSM.decreasing rank)
      |> GeneralTransitionSet.enum
      |> Enum.map (fun g -> g, GeneralTransition.start (LexRSM.decreasing rank))
    in

    let inc_det_sizebound v =
      entry_ts_to_decreasing |> TransitionSet.enum
      |> Enum.map (fun t -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
      |> Bound.maximum
      |> RealBound.of_intbound
    in

    let inc_prob_sizebound v =
      entry_gts_to_decreasing
      |> Enum.map (fun (g,l) -> Approximation.expsizebound_abs appr (g,l) v )
      |> RealBound.sum
    in

    let substituted_det_cost =
      GeneralTransition.cost (LexRSM.decreasing rank)
      |> RealBound.appr_substition_abs_all inc_det_sizebound
    in

    let subsistuted_prob_cost =
      GeneralTransition.cost (LexRSM.decreasing rank)
      |> RealBound.appr_substition_abs_all inc_prob_sizebound
    in

    let cost = mul_inctime_and_rhs (time,substituted_det_cost) |> RealBound.simplify_vars_nonnegative in
    let cost_nonprob_size = mul_inctime_and_rhs (time_nonprob_size, subsistuted_prob_cost) in

    (time, cost, cost_nonprob_size)

  in Logger.with_log logger Logger.DEBUG
       (fun () -> "compute_bound", ["rank", LexRSM.pprf_to_string rank])
                     ~result:(fun (time,cost,cost_nonprob_size) -> "time: "^RealBound.to_string time ^
                          " cost (with nonprob costfunc and prob time): " ^ (RealBound.to_string cost) ^" cost (with prob costfunc and nonprob time): "^RealBound.to_string cost_nonprob_size)
                     execute

let improve_with_rank add_exptimebound add_expcostbound program appr (rank: LexRSM.t) =
  let (time,cost,cost_nonprob_size) = compute_bounds appr program rank in
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
  |> (fun mca ->
        if Approximation.is_expcost_bounded appr (LexRSM.decreasing rank) || RealBound.is_infinity cost_nonprob_size then
          mca
        else
          MaybeChanged.(mca >>= (changed % add_expcostbound cost_nonprob_size (LexRSM.decreasing rank)))
     )

(** Checks if a transition is bounded *)
let exp_bounded appr transition =
  Approximation.is_expcost_bounded appr transition && Approximation.is_exptime_bounded appr transition

let improve add_exptimebound add_expcostbound cache program appr =
  program
  |> Program.non_trivial_transitions
  |> GeneralTransitionSet.of_transitionset
  |> GeneralTransitionSet.filter (not % exp_bounded appr)
  |> GeneralTransitionSet.enum
  |> MaybeChanged.fold_enum (fun appr gt ->
         LexRSM.find cache program gt
         |> Option.map_default (fun rank ->
              improve_with_rank add_exptimebound add_expcostbound program appr rank
            ) (MaybeChanged.return appr)
       ) appr
