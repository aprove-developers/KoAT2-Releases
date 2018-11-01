open Batteries
open BoundsInst
open ProgramTypes
open Polynomials
   
let rec find_bounds_ (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> SizeBounds.improve program
  |> RankingBounds.improve `Time program
  |> MaybeChanged.if_changed (find_bounds_ program)
  |> MaybeChanged.unpack

let find_bounds (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> TrivialTimeBounds.compute program
  |> find_bounds_ program
  |> (fun appr ->
    if program |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_one (Transition.cost t))) then
      RankingBounds.improve `Cost program appr
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
    |> RealBound.of_intbound
  in

  Program.generalized_transitions program
  |> fun gtset -> GeneralTransitionSet.fold
       (fun gt appr -> Approximation.add_exptimebound (get_gt_timebound gt) gt appr)
       gtset
       appr

(* lifts nonprobabilistic sizebounds of result variables to expected size bounds for the corresponding expected result variables.*)
let lift_nonprob_sizebounds program appr = 
  let get_gtl_sizebound kind ((gt,l),v) = 
    GeneralTransition.transitions gt
    |> TransitionSet.filter (Location.equal l % Transition.target)
    |> TransitionSet.enum
    |> Enum.map (fun t -> Approximation.sizebound kind appr t v)
    |> (match kind with 
       | `Upper -> Bound.maximum
       | `Lower -> Bound.minimum)
    |> RealBound.of_intbound
  in

  Program.generalized_transitions program
  |> GeneralTransitionSet.to_list
  |> List.map (fun gt -> List.cartesian_product [gt] (GeneralTransition.targets gt |> LocationSet.to_list))
  |> List.flatten
  |> List.map (flip List.cartesian_product (Program.vars program |> VarSet.to_list) % List.singleton)
  |> List.flatten
  |> List.cartesian_product [`Upper; `Lower]
  |> List.fold_left 
       (fun appr (kind,((gt,l),var)) -> Approximation.add_expsizebound kind (get_gtl_sizebound kind ((gt,l),var)) (gt,l) var appr) 
       appr
  
let rec find_exp_bounds_ (program: Program.t) (appr: Approximation.t): Approximation.t =
  ExpSizeBounds.improve program appr
  |> ExpRankingBounds.improve program
  |> MaybeChanged.if_changed (find_exp_bounds_ program)
  |> MaybeChanged.unpack

let find_exp_bounds (program: Program.t) (appr: Approximation.t): Approximation.t =
  appr
  |> TrivialTimeBounds.compute program
  |> find_bounds_ program 
  |> lift_nonprob_timebounds program 
  |> lift_nonprob_sizebounds program 
  |> find_exp_bounds_ program
