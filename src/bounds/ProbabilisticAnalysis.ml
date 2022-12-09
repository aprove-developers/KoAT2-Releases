open Batteries
open ProbabilisticProgramModules
open BoundsInst
open Approximation.Probabilistic

type configuration = { compute_refined_plrfs: bool }

let default_configuration = { compute_refined_plrfs = false }

let lift_bounds gts program_vars (class_appr, appr): ExpApproximation.t =
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

let improve_timebounds ~conf program scc (class_appr,appr): ExpApproximation.t MaybeChanged.t =
  let is_exptime_bounded = ExpApproximation.is_time_bounded appr in
  let unbounded_vars (gt,l) =
    Program.input_vars program
    |> VarSet.filter (RealBound.is_infinity % ExpApproximation.sizebound appr (gt,l))
  in
  let find_plrfs refined =
    GeneralTransitionSet.filter (not % is_exptime_bounded) scc
    |> GeneralTransitionSet.to_array
    |> Parmap.array_parmap (Plrf.find_scc ~refined program is_exptime_bounded unbounded_vars scc)
    |> Array.enum
    |> Util.cat_maybes_enum
  in
  (if conf.compute_refined_plrfs then List.enum [false; true] else List.enum [false])
  |> Enum.map find_plrfs
  |> Enum.filter (not % Enum.is_empty)
  |> fun en -> Enum.peek en |? Enum.empty ()
  |> MaybeChanged.fold_enum (fun appr -> PlrfBounds.improve_with_plrf program (class_appr,appr)) appr

let rec knowledge_propagation program scc appr_mc: ExpApproximation.t MaybeChanged.t =
  GeneralTransitionSet.filter (not % ExpApproximation.is_time_bounded (MaybeChanged.unpack appr_mc)) scc
  |> GeneralTransitionSet.enum
  |> fun gtset ->  MaybeChanged.(appr_mc >>= fun appr -> MaybeChanged.fold_enum (fun appr gt ->
      let new_bound =
        GeneralTransitionSet.enum (Program.pre_gt_cached program gt)
        |> Enum.map (ExpApproximation.timebound appr)
        |> RealBound.sum
      in
      if RealBound.is_finite new_bound then
        MaybeChanged.changed (ExpApproximation.add_timebound new_bound gt appr)
      else MaybeChanged.same appr
    ) appr gtset)
  |> fun appr_mc ->
      if MaybeChanged.has_changed appr_mc then knowledge_propagation program scc appr_mc else appr_mc

let rec improve_scc ~conf program scc (class_appr,appr) : ExpApproximation.t =
  improve_timebounds ~conf program scc (class_appr,appr)
  |> knowledge_propagation program scc
  |> fun mc ->
      if MaybeChanged.has_changed mc then
        improve_scc ~conf program scc (class_appr, MaybeChanged.unpack mc)
      else MaybeChanged.unpack mc

let perform_analysis ?(conf=default_configuration) program class_appr: ExpApproximation.t =
  let gts = Program.gts program in
  let program_vars = Program.vars program in
  let sccs = Program.sccs_gts program in

  lift_bounds gts program_vars (class_appr, ExpApproximation.create program)
  |> fun appr -> Enum.fold (fun appr scc -> improve_scc ~conf program scc (class_appr,appr)) appr sccs
