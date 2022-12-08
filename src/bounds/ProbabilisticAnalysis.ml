open Batteries
open ProbabilisticProgramModules
open BoundsInst
open Approximation.Probabilistic

let lift_bounds class_appr gts program_vars appr: ExpApproximation.t =
  let lift_time_bounds appr =
    let gt_timebound gt =
      TransitionSet.enum (GeneralTransition.transitions gt)
      |> Enum.map (ClassicApproximation.timebound class_appr)
      |> RealBound.of_intbound % Bound.sum
    in
    GeneralTransitionSet.enum gts
    |> Enum.fold (fun appr gt -> ExpApproximation.add_timebound (gt_timebound gt) gt appr) appr
  in

  let lift_size_bounds appr =
    let get_gt_rvs gt v =
      LocationSet.enum (GeneralTransition.targets gt)
      |> Enum.map (fun l -> (gt,l),v)
    in
    let rv_sizebound ((gt,l),v) =
      TransitionSet.enum (GeneralTransition.transitions gt)
      |> Enum.map (fun t -> ClassicApproximation.sizebound class_appr t v)
      |> RealBound.of_intbound % Bound.sum
    in
    List.enum (List.cartesian_product (GeneralTransitionSet.to_list gts) (VarSet.to_list program_vars))
    |> Enum.flatten % Enum.map (fun (gt,v) -> get_gt_rvs gt v)
    |> Enum.fold (fun appr (rvt,v) -> ExpApproximation.add_sizebound (rv_sizebound (rvt,v)) rvt v appr) appr
  in
  lift_size_bounds (lift_time_bounds appr)


let perform_analysis program class_appr: ExpApproximation.t =
  let gts = Program.gts program in
  let program_vars = Program.vars program in
  lift_bounds class_appr gts program_vars (ExpApproximation.create program)
