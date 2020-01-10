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
let lift_nonprob_timebounds simplify_smt program appr =
  let get_gt_timebound gt =
    GeneralTransition.transitions gt
    |> TransitionSet.enum
    |> Enum.map (Approximation.timebound appr)
    |> Bound.sum
    |> Bound.simplify_vars_nonnegative
  in

  Program.generalized_transitions program
  |> fun gtset -> GeneralTransitionSet.fold
       (fun gt appr ->
          let timebound = get_gt_timebound gt in
          if Bound.is_infinity timebound then
            appr
          else
            Approximation.add_exptimebound simplify_smt (RealBound.of_intbound timebound) gt appr
            |> Approximation.add_timebound_gt timebound gt
            |> Approximation.add_expcostbound simplify_smt
                ( RealBound.(of_intbound timebound * appr_substition_abs_all (BoundsHelper.nonprob_incoming_size program appr gt) (GeneralTransition.cost gt))
                |> RealBound.simplify_vars_nonnegative)
                gt
       )
       gtset
       appr

(* lifts nonprobabilistic sizebounds of result variables to expected size bounds for the corresponding expected result variables.*)
let lift_nonprob_sizebounds simplify_smt program appr =
  let get_gtl_sizebound ((gt,l),v) =
    let transitions =
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal l % Transition.target)
    in
    transitions
    |> TransitionSet.enum
    |> Enum.map
        (fun t ->
          Bound.abs_bound (fun kind -> Approximation.sizebound kind appr t v)
          |> RealBound.of_intbound
       )
    |> RealBound.maximum
    |> fun b -> RealBound.(b * (of_constant @@ TransitionSet.total_probability transitions))
    |> RealBound.simplify_vars_nonnegative
  in

  Program.generalized_transitions program
  |> GeneralTransitionSet.to_list
  |> List.map (fun gt -> List.cartesian_product [gt] (GeneralTransition.targets gt |> LocationSet.to_list))
  |> List.flatten
  |> List.map (flip List.cartesian_product (Program.vars program |> VarSet.to_list) % List.singleton)
  |> List.flatten
  |> List.fold_left
       (fun appr ((gt,l),var) -> Approximation.add_expsizebound simplify_smt (get_gtl_sizebound ((gt,l),var)) (gt,l) var appr)
       appr

let rec find_exp_bounds_ simplify_smt cache ervg sccs (program: Program.t) (appr: Approximation.t): Approximation.t =
  ExpSizeBounds.improve simplify_smt (CacheManager.elsb_cache cache) ervg sccs program appr
  |> ExpRankingBounds.improve (Approximation.add_exptimebound simplify_smt) (Approximation.add_expcostbound simplify_smt) (CacheManager.lrsm_cache cache) program
  |> MaybeChanged.if_changed (find_exp_bounds_ simplify_smt cache ervg sccs program)
  |> MaybeChanged.unpack

(* add costbounds of 0 for all general transitions with cost 0 *)
let add_trivial_expcostbounds program appr =
  Program.generalized_transitions program
  |> GeneralTransitionSet.enum
  |> Enum.filter (RealBound.equal RealBound.zero % GeneralTransition.cost)
  |> Enum.fold (flip (Approximation.add_expcostbound false RealBound.zero)) appr

let rec find_exp_bounds simplify_smt ~generate_invariants_bottom_up (use_bottom_up: bool) (cache: CacheManager.t)
(program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
  let ervg = ERVG.rvg (CacheManager.elsb_cache cache) program in
  let sccs =
    let module C = Graph.Components.Make(ERVG) in
    List.rev @@ C.scc_list ervg
  in

  appr
  |> TrivialTimeBounds.compute program
  |> TrivialTimeBounds.compute_generaltransitions program
  |> add_trivial_expcostbounds program
  |> find_bounds_ cache program
  |> lift_nonprob_timebounds simplify_smt program
  |> lift_nonprob_sizebounds simplify_smt program
  |> find_exp_bounds_ simplify_smt cache ervg sccs program
  (* Now apply bottom-up if there is an SCC without an expected cost bound *)
  |> fun appr ->
    if use_bottom_up then
      continue_with_bottom_up simplify_smt ~generate_invariants_bottom_up:generate_invariants_bottom_up (CacheManager.trans_id_counter cache) program appr
    else
      (program, appr)

and continue_with_bottom_up simplify_smt ~generate_invariants_bottom_up trans_id_counter program appr =
  let logger        = Logging.(get BottomUp) in
  let new_cache     = CacheManager.new_cache_with_counter trans_id_counter () in
  let bottom_up_res =
    BottomUpHelper.perform_bottom_up
      ~generate_invariants:generate_invariants_bottom_up
      ~find_exp_bounds:(find_exp_bounds simplify_smt ~generate_invariants_bottom_up:generate_invariants_bottom_up false)
      (CacheManager.trans_id_counter new_cache)
      program appr
  in

  Option.map_default
    (fun (new_prog, new_appr) ->
      Logger.log logger Logger.DEBUG (fun () -> "continue recursively", ["new_prog", Program.to_string ~show_gtcost:true new_prog]);
      find_exp_bounds simplify_smt ~generate_invariants_bottom_up:generate_invariants_bottom_up true new_cache new_prog new_appr)
    (program,appr) bottom_up_res
