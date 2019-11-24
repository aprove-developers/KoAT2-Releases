open Batteries
open BoundsInst
open ProgramTypes
open Polynomials

let rec find_bounds_ (cache: CacheManager.t) (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> SizeBounds.improve (CacheManager.lsb_cache cache) program
  |> RankingBounds.improve (CacheManager.ranking_cache cache) `Time program
  |> MaybeChanged.if_changed (find_bounds_ cache program)
  |> MaybeChanged.unpack

let find_bounds (cache: CacheManager.t) (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> TrivialTimeBounds.compute program
  |> find_bounds_ cache program
  |> (fun appr ->
    if program |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_one (Transition.cost t))) then
      RankingBounds.improve (CacheManager.ranking_cache cache) `Cost program appr
    else
      MaybeChanged.same appr
  )
  |> MaybeChanged.unpack
  |> CostBounds.infer_from_timebounds program

(* lifts nonprobabilistic timebounds to expected time bounds for general transitions *)
let lift_nonprob_timebounds program appr =
  let get_gt_timebound gt =
    GeneralTransition.transitions gt
    |> TransitionSet.enum
    |> Enum.map (Approximation.timebound appr)
    |> Bound.sum
  in

  Program.generalized_transitions program
  |> fun gtset -> GeneralTransitionSet.fold
       (fun gt appr ->
          let timebound = get_gt_timebound gt in
          Approximation.add_exptimebound (RealBound.of_intbound timebound) gt appr
          |> Approximation.add_timebound_gt timebound gt
       )
       gtset
       appr

(* lifts nonprobabilistic sizebounds of result variables to expected size bounds for the corresponding expected result variables.*)
let lift_nonprob_sizebounds program appr =
  let get_gtl_sizebound ((gt,l),v) =
    let transitions =
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal l % Transition.target)
    in
    transitions
    |> TransitionSet.enum
    |> Enum.map (fun t kind -> Approximation.sizebound kind appr t v)
    |> Enum.map Bound.abs_bound
    |> Bound.maximum
    |> RealBound.of_intbound
    |> fun b -> RealBound.(b * (of_constant @@ TransitionSet.total_probability transitions))
  in

  Program.generalized_transitions program
  |> GeneralTransitionSet.to_list
  |> List.map (fun gt -> List.cartesian_product [gt] (GeneralTransition.targets gt |> LocationSet.to_list))
  |> List.flatten
  |> List.map (flip List.cartesian_product (Program.vars program |> VarSet.to_list) % List.singleton)
  |> List.flatten
  |> List.fold_left
       (fun appr ((gt,l),var) -> Approximation.add_expsizebound (get_gtl_sizebound ((gt,l),var)) (gt,l) var appr)
       appr

let rec find_exp_bounds_ cache ervg sccs (program: Program.t) (appr: Approximation.t): Approximation.t =
  ExpSizeBounds.improve (CacheManager.elsb_cache cache) ervg sccs program appr
  |> ExpRankingBounds.improve (CacheManager.lrsm_cache cache) program
  |> MaybeChanged.if_changed (find_exp_bounds_ cache ervg sccs program)
  |> MaybeChanged.unpack

let find_exp_bounds (cache: CacheManager.t) (program: Program.t) (appr: Approximation.t): Approximation.t =
  let ervg = ERVG.rvg (CacheManager.elsb_cache cache) program in
  let sccs =
    let module C = Graph.Components.Make(ERVG) in
    List.rev @@ C.scc_list ervg
  in

  appr
  |> TrivialTimeBounds.compute program
  |> TrivialTimeBounds.compute_generaltransitions program
  |> find_bounds_ cache program
  |> lift_nonprob_timebounds program
  |> lift_nonprob_sizebounds program
  |> find_exp_bounds_ cache ervg sccs program
