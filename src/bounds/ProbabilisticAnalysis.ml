open Batteries
open ProbabilisticProgramModules
open BoundsInst
open Approximation.Probabilistic

type configuration = { compute_refined_plrfs: bool }

let default_configuration = { compute_refined_plrfs = false }

let size_logger = Logging.(get ExpSize)

let rvts_from_gts gts =
  GeneralTransitionSet.enum gts
  |> Enum.map (fun gt -> GeneralTransition.targets gt |> LocationSet.enum |> Enum.map (fun l -> gt,l))
  |> List.of_enum % Enum.flatten

let grvs_from_gts_and_vars gts vars =
  rvts_from_gts gts
  |> fun rvts -> List.cartesian_product rvts (VarSet.to_list vars)

let lift_bounds gts program_vars (class_appr, appr): ExpApproximation.t =
  let lift_time_bounds appr =
    let gt_timebound gt =
      TransitionSet.enum (GeneralTransition.transitions gt)
      |> Enum.map (ClassicalApproximation.timebound class_appr)
      |> RealBound.of_intbound % Bound.sum
    in
    GeneralTransitionSet.enum gts
    |> Enum.fold (fun appr gt -> ExpApproximation.add_timebound (gt_timebound gt) gt appr) appr
  in

  let lift_size_bounds appr =
    let rv_sizebound ((gt,l),v) =
      TransitionSet.enum (GeneralTransition.transitions gt)
      |> Enum.map (fun t -> ClassicalApproximation.sizebound class_appr t v)
      |> RealBound.of_intbound % Bound.sum
    in
    grvs_from_gts_and_vars gts program_vars
    |> List.fold (fun appr (rvt,v) -> ExpApproximation.add_sizebound (rv_sizebound (rvt,v)) rvt v appr) appr
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

let knowledge_propagation program scc appr_mc: ExpApproximation.t MaybeChanged.t =
  let rec iter appr =
    GeneralTransitionSet.filter (not % ExpApproximation.is_time_bounded (MaybeChanged.unpack appr_mc)) scc
    |> GeneralTransitionSet.enum
    |> fun gtset ->  MaybeChanged.fold_enum (fun appr gt ->
        let new_bound =
          GeneralTransitionSet.enum (Program.pre_gt_cached program gt)
          |> Enum.map (ExpApproximation.timebound appr)
          |> RealBound.sum
        in
        if RealBound.is_finite new_bound then
          MaybeChanged.changed (ExpApproximation.add_timebound new_bound gt appr)
        else MaybeChanged.same appr
      ) appr gtset
    |> fun appr_mc ->
        if MaybeChanged.has_changed appr_mc then MaybeChanged.(appr_mc >>= iter) else appr_mc
  in
  MaybeChanged.(appr_mc >>= iter)


module ELCBMap = Map.Make(GRV.RVTuple_)

(* TODO rvs_in instead of rvs_out  *)
let improve_sizebounds program program_vars scc (rvts_scc,rvs_in) elcbs (class_appr,appr): ExpApproximation.t =
  let trivial_sizebounds appr =
    let trivial_sizebound_for_grv ((gt,l),v) =
      let pre_gt = Program.pre_gt_cached program gt in
      let start_loc = GeneralTransition.src gt in
      let pre_size_exp v =
        GeneralTransitionSet.enum pre_gt
        |> Enum.map (fun pre_gt -> ExpApproximation.sizebound appr (pre_gt, start_loc) v)
        |> RealBound.sum
      in
      let pre_size_classical v =
        GeneralTransitionSet.enum pre_gt
        |> Enum.map (TransitionSet.enum
                     % TransitionSet.filter (Location.equal start_loc % Transition.target)
                     % GeneralTransition.transitions)
        |> Enum.flatten
        |> Enum.map (fun t -> ClassicalApproximation.sizebound class_appr t v)
        |> RealBound.of_intbound % Bound.sum
      in
      let var_overapprox = if Program.is_initial_gt program gt then RealBound.of_var v else pre_size_exp v in
      let elcb = ELCBMap.find ((gt,l),v) elcbs in
      let elcb_overapprox =
        if Program.is_initial_gt program gt then RealBound.of_var v
        else
          if RealBound.is_linear elcb then RealBound.substitute_f pre_size_exp elcb
          else RealBound.substitute_f pre_size_classical elcb
      in
      RealBound.add var_overapprox elcb_overapprox
      |> tap (fun r -> Logger.log size_logger Logger.DEBUG (fun () -> "trivial_sizebound_for_rv", [ "grv", GRV.to_id_string ((gt,l),v)
                                                                                             ; "var_overapprox", RealBound.to_string var_overapprox
                                                                                             ; "elcb", RealBound.to_string elcb
                                                                                             ; "elcb_overapprox", RealBound.to_string elcb_overapprox
                                                                                             ; "result", RealBound.to_string r]))
    in
    List.fold (fun appr (rvt,v) ->
        ExpApproximation.add_sizebound (trivial_sizebound_for_grv (rvt,v)) rvt v appr
      ) appr rvs_in
  in

  let nontrivial_sizebounds appr =
    let entry_rvts = List.of_enum (BoundsHelper.entry_gts_with_locs program scc) in
    let overappr_var_in gt v =
      GeneralTransition.transitions gt
      |> Program.pre_transitionset_cached program % TransitionSet.any
      |> Enum.map (fun t -> ClassicalApproximation.sizebound class_appr t v) % TransitionSet.enum
      |> RealBound.of_intbound % Bound.sum
    in
    let nontrivial_sizebound appr v =
      let execute = fun () ->
        let start_value =
          List.enum entry_rvts
          |> Enum.map (fun rvt -> ExpApproximation.sizebound appr rvt v)
          |> RealBound.sum
          |> tap (fun s -> Logger.log size_logger Logger.DEBUG (fun () -> "start_value", ["res", RealBound.to_string s]))
        in
        let acc_change_grv (gt,l) =
          let elcb = ELCBMap.find ((gt,l),v) elcbs in
          let change_bound = RealBound.substitute_f (overappr_var_in gt) elcb in
          RealBound.(change_bound * ExpApproximation.timebound appr gt)
          |> tap (fun r -> Logger.log size_logger Logger.DEBUG (fun () -> "acc_change_rvt", [ "rv", GRV.to_id_string ((gt,l),v)
                                                                                      ; "elcb", RealBound.to_string elcb
                                                                                      ; "res", RealBound.to_string r
                                                                                      ; "change_bound", RealBound.to_string change_bound
                                                                                      ; "time_bound", RealBound.to_string (ExpApproximation.timebound appr gt)]))
        in
        let acc_change =
          List.enum rvts_scc
          |> Enum.map acc_change_grv
          |> RealBound.sum
        in
        RealBound.(start_value + acc_change)
      in
      Logger.with_log size_logger Logger.DEBUG
        (fun () -> "nontrivial_sizebound", [ "scc", GeneralTransitionSet.to_id_string scc; "v", Var.to_string v])
        ~result:RealBound.to_string
        execute
    in
    VarSet.fold (fun v appr ->
        let new_bound = nontrivial_sizebound appr v in
        ExpApproximation.add_sizebounds new_bound (List.map (fun rvt -> rvt,v) rvts_scc) appr
      )
      program_vars appr

  in
  trivial_sizebounds appr
  |> nontrivial_sizebounds


let improve_scc ~conf program program_gts program_vars scc (class_appr,appr) : ExpApproximation.t =
  let rvts_scc = rvts_from_gts scc in
  let rvs_in =
    List.cartesian_product
      (List.of_enum @@ BoundsHelper.entry_gts_with_locs program scc)
      (VarSet.to_list program_vars)
  in
  let elcbs =
    List.cartesian_product rvts_scc (VarSet.to_list program_vars)
    |> List.append rvs_in
    |> Enum.map (fun rv -> rv, ExpectedLocalChangeBound.compute_elcb program_vars rv) % List.enum
    |> ELCBMap.of_enum
  in
  let rec improve_scc_ appr =
    improve_sizebounds program program_vars scc (rvts_scc,rvs_in) elcbs (class_appr,appr)
    |> fun appr -> improve_timebounds ~conf program scc (class_appr,appr)
    |> knowledge_propagation program scc
    |> MaybeChanged.map (fun appr -> improve_sizebounds program program_vars scc (rvts_scc,rvs_in) elcbs (class_appr,appr))
    |> fun mc ->
        if MaybeChanged.has_changed mc then
          improve_scc_ (MaybeChanged.unpack mc)
        else MaybeChanged.unpack mc
  in
  improve_scc_ appr

let perform_analysis ?(conf=default_configuration) program class_appr: ExpApproximation.t =
  let gts = Program.gts program in
  let program_vars = Program.vars program in
  let sccs = Program.sccs_gts program in

  lift_bounds gts program_vars (class_appr, ExpApproximation.create program)
  |> fun appr ->
      Enum.fold
        (fun appr scc_with_locs -> improve_scc ~conf program gts program_vars scc_with_locs (class_appr,appr))
        appr sccs