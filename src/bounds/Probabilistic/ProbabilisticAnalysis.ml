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


module TrivialTimeBounds = TrivialTimeBounds.Make (Bound) (NonProbOverappr)

let trivial_time_bounds program class_appr =
  let overappr_program = Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program in
  coerce_from_classical_approximation class_appr
  |> TrivialTimeBounds.compute overappr_program
  |> coerce_from_nonprob_overappr_approximation


let lift_bounds program program_vars gts apprs : ExpApproximation.t =
  let lift_time_bounds appr =
    let gt_timebound gt =
      Set.to_sequence (GeneralTransition.transitions gt)
      |> Sequence.map ~f:(ClassicalApproximation.timebound apprs.class_appr)
      |> RationalBound.of_intbound % Bound.sum
    in
    Set.to_sequence gts
    |> Sequence.fold ~f:(fun appr gt -> ExpApproximation.add_timebound (gt_timebound gt) gt appr) ~init:appr
  in

  let lift_size_bounds appr =
    let rv_sizebound ((gt, l), v) =
      Set.to_sequence (GeneralTransition.transitions gt)
      |> Sequence.map ~f:(fun t -> ClassicalApproximation.sizebound apprs.class_appr t v)
      |> RationalBound.of_intbound % Bound.sum
    in
    grvs_from_gts_and_vars gts program_vars
    |> List.fold
         ~f:(fun appr (rvt, v) -> ExpApproximation.add_sizebound (rv_sizebound (rvt, v)) rvt v appr)
         ~init:appr
  in
  lift_size_bounds (lift_time_bounds apprs.appr)
  |> tap (fun appr ->
         ProofOutput.add_to_proof
           FormattedString.(
             fun () ->
               mk_str_header_small "Classical Approximation after Lifting Classical Results"
               <> reduce_header_sizes ~levels_to_reduce:2
                    (ExpApproximation.to_formatted ~pretty:true program appr)))


let improve_timebounds twn_state
    ~(classic_conf : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration)
    ~conf program scc apprs =
  let open MaybeChanged.Monad in
  let* appr =
    PlrfBounds.improve_timebounds_plrf ~compute_refined_plrfs:conf.compute_refined_plrfs program scc
      (apprs.class_appr, apprs.appr)
  in
  IntegrateClassicalAnalysis.improve ~twn:twn_state ~mprf_depth:classic_conf.run_mprf_depth program scc
    (apprs.class_appr, appr)


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


let improve_sizebounds program program_vars scc (rvts_scc, grvs_in_and_out) elcbs apprs =
  ProbabilisticSizeBounds.trivial_sizebounds program ~grvs_in_and_out elcbs apprs.class_appr apprs.appr
  |> ProbabilisticSizeBounds.nontrivial_sizebounds program ~program_vars ~scc ~rvts_scc elcbs apprs.class_appr
  |> ProbabilisticSizeBounds.propagate_sizes program ~program_vars ~rvts_scc apprs.class_appr


module ELCBMap = MakeMapCreators1 (GRV)
module ClassicAnalysis = Analysis.Make (Bound) (NonProbOverappr)

let improve_scc_classically
    ~(classic_conf : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration)
    program program_vars scc_locs apprs =
  ProofOutput.start_new_subproof ();

  let scc_with_in_and_out = Program.scc_gts_from_locs_with_incoming_and_outgoing program scc_locs in
  let overappr_classical_program =
    Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program
  in
  let class_appr =
    coerce_from_classical_approximation apprs.class_appr
    |> ClassicAnalysis.improve_scc ~conf:classic_conf scc_locs overappr_classical_program
    |> coerce_from_nonprob_overappr_approximation
  in
  let appr = lift_bounds program program_vars scc_with_in_and_out { apprs with class_appr } in
  let subproof = ProofOutput.get_subproof () in

  ProofOutput.add_to_proof_with_format
    FormattedString.(
      fun fmt ->
        mk_str_header_big ("Run classical analysis on SCC: " ^ LocationSet.to_string scc_locs)
        <> ProofOutput.LocalProofOutput.get_proof subproof fmt);
  { class_appr; appr }


let improve_scc_probabilistically
    ~(classic_conf : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.analysis_configuration)
    ~conf program program_vars scc_locs apprs : ExpApproximation.t =
  ProofOutput.start_new_subproof ();

  let scc = Program.scc_gts_from_locs program scc_locs in
  let rvts_in_and_out =
    Set.diff (Program.scc_gts_from_locs_with_incoming_and_outgoing program scc_locs) scc |> rvts_from_gts
  in
  let rvts_scc = rvts_from_gts scc in
  let grvs_in_and_out = List.cartesian_product rvts_in_and_out (Set.to_list program_vars) in
  let elcbs =
    List.cartesian_product rvts_scc (Set.to_list program_vars)
    |> List.append grvs_in_and_out
    |> Sequence.map ~f:(fun rv -> (rv, ExpectedLocalChangeBound.compute_elcb program_vars rv))
       % Sequence.of_list
    |> ELCBMap.of_sequence_exn
  in
  let twn_state = (ref @@ IntegrateClassicalAnalysis.initial_twn_state program scc, classic_conf.twn) in
  let improve_scc_ appr =
    let open MaybeChanged.Monad in
    let appr =
      improve_sizebounds program program_vars scc (rvts_scc, grvs_in_and_out) elcbs { apprs with appr }
    in
    let* appr = improve_timebounds twn_state ~classic_conf ~conf program scc { apprs with appr } in
    let+ appr = knowledge_propagation program scc appr in
    let appr =
      improve_sizebounds program program_vars scc (rvts_scc, grvs_in_and_out) elcbs { apprs with appr }
    in
    appr
  in
  let appr = Util.find_fixpoint improve_scc_ apprs.appr in

  let subproof = ProofOutput.get_subproof () in
  ProofOutput.add_to_proof_with_format
    FormattedString.(
      fun fmt ->
        FormattedString.mk_str_header_big
          ("Run probabilistic analysis on SCC: " ^ LocationSet.to_string scc_locs)
        <> ProofOutput.LocalProofOutput.get_proof subproof fmt);
  appr


let improve_scc ~classic_conf ~conf program program_vars scc_locs apprs =
  (* Compute trivial time bounds *)
  let apprs = improve_scc_classically ~classic_conf program program_vars scc_locs apprs in
  let appr = improve_scc_probabilistically ~classic_conf ~conf program program_vars scc_locs apprs in
  { apprs with appr }


let lift_appr_to_exp_costbounds program apprs =
  let gts = Program.gts program in
  Set.fold gts ~init:apprs ~f:(fun apprs gt ->
      let gtcost =
        Set.to_sequence (GeneralTransition.transitions gt)
        |> Sequence.map ~f:(fun (_, label, _) ->
               let cbound = RationalBound.of_intpoly (TransitionLabel.cost label) in
               RationalBound.substitute_f
                 (ProbabilisticSizeBounds.get_pre_size_classical program apprs.class_appr gt)
                 cbound)
        |> RationalBound.sum
      in
      let new_bound = RationalBound.mul gtcost (ExpApproximation.timebound apprs.appr gt) in
      { apprs with appr = ExpApproximation.add_costbound new_bound gt apprs.appr })


let perform_analysis ?(classic_conf = Analysis.default_configuration) ?(conf = default_configuration) program
    =
  let program_vars = Program.input_vars program in
  let sccs = Program.sccs_locs program in

  let apprs =
    { appr = ExpApproximation.empty; class_appr = trivial_time_bounds program ClassicalApproximation.empty }
  in
  let apprs =
    List.fold
      ~f:(fun apprs scc_locs -> improve_scc ~classic_conf ~conf program program_vars scc_locs apprs)
      ~init:apprs sccs
    |> lift_appr_to_exp_costbounds program
  in
  (program, apprs)
