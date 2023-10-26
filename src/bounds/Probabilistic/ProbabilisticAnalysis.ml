open! OurBase
open ProbabilisticProgramModules
open Bounds
open Approximation.Probabilistic

type configuration = { compute_refined_plrfs : bool }

let default_configuration = { compute_refined_plrfs = false }

let rvts_from_gts gts =
  Set.to_sequence gts
  |> Sequence.map ~f:(fun gt ->
         GeneralTransition.targets gt |> Set.to_sequence |> Sequence.map ~f:(fun l -> (gt, l)))
  |> Sequence.to_list % Sequence.join


let grvs_from_gts_and_vars gts vars =
  rvts_from_gts gts |> fun rvts -> List.cartesian_product rvts (Set.to_list vars)


let lift_bounds program program_vars (class_appr, appr) : ExpApproximation.t =
  let program_gts = Program.gts program in
  let lift_time_bounds appr =
    let gt_timebound gt =
      Set.to_sequence (GeneralTransition.transitions gt)
      |> Sequence.map ~f:(ClassicalApproximation.timebound class_appr)
      |> RationalBound.of_intbound % Bound.sum
    in
    Set.to_sequence program_gts
    |> Sequence.fold ~f:(fun appr gt -> ExpApproximation.add_timebound (gt_timebound gt) gt appr) ~init:appr
  in

  let lift_size_bounds appr =
    let rv_sizebound ((gt, l), v) =
      Set.to_sequence (GeneralTransition.transitions gt)
      |> Sequence.map ~f:(fun t -> ClassicalApproximation.sizebound class_appr t v)
      |> RationalBound.of_intbound % Bound.sum
    in
    grvs_from_gts_and_vars program_gts program_vars
    |> List.fold
         ~f:(fun appr (rvt, v) -> ExpApproximation.add_sizebound (rv_sizebound (rvt, v)) rvt v appr)
         ~init:appr
  in
  lift_size_bounds (lift_time_bounds appr)
  |> tap (fun appr ->
         ProofOutput.add_to_proof
           FormattedString.(
             fun () ->
               mk_str_header_small "Results obtained by lifting Classical Analysis"
               <> reduce_header_sizes ~levels_to_reduce:2
                    (ExpApproximation.to_formatted ~pretty:true program appr)))


let improve_timebounds twn_state
    ~(classic_conf : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration)
    ~conf program scc (class_appr, appr) =
  let open MaybeChanged.Monad in
  let* appr =
    PlrfBounds.improve_timebounds_plrf ~compute_refined_plrfs:conf.compute_refined_plrfs program scc
      (class_appr, appr)
  in
  IntegrateClassicalAnalysis.improve ~twn:twn_state ~mprf_depth:classic_conf.run_mprf_depth program scc
    (class_appr, appr)


(** Propagate time bounds of incoming general transitions *)
let knowledge_propagation program scc appr : ExpApproximation.t MaybeChanged.t =
  let iter appr =
    Set.filter ~f:(not % ExpApproximation.is_time_bounded appr) scc |> Set.to_sequence |> fun gtset ->
    MaybeChanged.fold_sequence
      ~f:(fun appr gt ->
        let new_bound =
          Set.to_sequence (Program.pre_gt program gt)
          |> Sequence.map ~f:(ExpApproximation.timebound appr)
          |> RationalBound.sum
        in
        if RationalBound.is_finite new_bound then (
          ProofOutput.add_str_paragraph_to_proof (fun () ->
              "knowledge_propagation leads to new time bound "
              ^ RationalBound.to_string ~pretty:true new_bound
              ^ " for transition "
              ^ GeneralTransition.to_string_pretty gt);
          MaybeChanged.changed (ExpApproximation.add_timebound new_bound gt appr))
        else
          MaybeChanged.same appr)
      ~init:appr gtset
  in
  Util.find_fixpoint_mc iter appr


let improve_sizebounds program program_vars scc (rvts_scc, grvs_in) elcbs (class_appr, appr) =
  ProbabilisticSizeBounds.trivial_sizebounds program ~grvs_in elcbs class_appr appr
  |> ProbabilisticSizeBounds.nontrivial_sizebounds program ~program_vars ~scc ~rvts_scc elcbs class_appr
  |> ProbabilisticSizeBounds.propagate_sizes program ~program_vars ~rvts_scc class_appr


module ELCBMap = MakeMapCreators1 (GRV)

let improve_scc
    ~(classic_conf : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration)
    ~conf program program_vars scc (class_appr, appr) : ExpApproximation.t =
  let rvts_scc = rvts_from_gts scc in
  let rvs_in =
    List.cartesian_product
      (Sequence.to_list @@ BoundsHelper.entry_gts_with_locs program scc)
      (Set.to_list program_vars)
  in
  let elcbs =
    List.cartesian_product rvts_scc (Set.to_list program_vars)
    |> List.append rvs_in
    |> Sequence.map ~f:(fun rv -> (rv, ExpectedLocalChangeBound.compute_elcb program_vars rv))
       % Sequence.of_list
    |> ELCBMap.of_sequence_exn
  in
  let twn_state = (ref @@ IntegrateClassicalAnalysis.initial_twn_state program scc, classic_conf.twn) in
  let improve_scc_ appr =
    let open MaybeChanged.Monad in
    let appr = improve_sizebounds program program_vars scc (rvts_scc, rvs_in) elcbs (class_appr, appr) in
    let* appr = improve_timebounds twn_state ~classic_conf ~conf program scc (class_appr, appr) in
    let+ appr = knowledge_propagation program scc appr in
    let appr = improve_sizebounds program program_vars scc (rvts_scc, rvs_in) elcbs (class_appr, appr) in
    appr
  in
  Util.find_fixpoint improve_scc_ appr


let lift_appr_to_exp_costbounds program class_appr appr =
  let gts = Program.gts program in
  Set.fold gts ~init:appr ~f:(fun appr gt ->
      let gtcost =
        Set.to_sequence (GeneralTransition.transitions gt)
        |> Sequence.map ~f:(fun (_, label, _) ->
               let cbound = RationalBound.of_intpoly (TransitionLabel.cost label) in
               RationalBound.substitute_f
                 (ProbabilisticSizeBounds.get_pre_size_classical program class_appr gt)
                 cbound)
        |> RationalBound.sum
      in
      let new_bound = RationalBound.mul gtcost (ExpApproximation.timebound appr gt) in
      ExpApproximation.add_costbound new_bound gt appr)


let perform_analysis ?(classic_conf = Analysis.default_configuration) ?(conf = default_configuration) program
    class_appr : ExpApproximation.t =
  let program_vars = Program.input_vars program in
  let sccs = Program.sccs_gts program in

  let appr = lift_bounds program program_vars (class_appr, ExpApproximation.empty) in
  List.fold
    ~f:(fun appr scc_with_locs ->
      improve_scc ~classic_conf ~conf program program_vars scc_with_locs (class_appr, appr))
    ~init:appr sccs
  |> lift_appr_to_exp_costbounds program class_appr


module ClassicAnalysis = Analysis.Make (Bound) (NonProbOverappr)

let perform_classic_and_probabilistic_analysis ?(classic_conf = Analysis.default_configuration)
    ?(conf = default_configuration) (program : Program.t) =
  let overappr_classical_program =
    Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program
  in

  let program, class_appr =
    ClassicAnalysis.improve ~preprocess:identity ~conf:classic_conf overappr_classical_program
      NonProbOverapprApproximation.empty
    (* preprocess is only needed for CFR currently *)
    |> Tuple2.map
         Type_equal.(conv (sym ProbabilisticPrograms.Equalities.program_equalities))
         coerce_from_nonprob_overappr_approximation
  in

  let prob_appr = perform_analysis ~classic_conf program class_appr in

  (program, (class_appr, prob_appr))
