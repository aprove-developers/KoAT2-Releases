open! OurBase
open ProbabilisticProgramModules
open Bounds
open Approximation.Probabilistic

type configuration = {
  classical_local : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.local_configuration;
  cfrs : CFR.Probabilistic.cfr List.t;
}

let default_configuration = { cfrs = []; classical_local = Analysis.default_local_configuration }

let rvts_from_gts gts =
  Set.to_sequence gts
  |> Sequence.map ~f:(fun gt ->
         GeneralTransition.targets gt |> Set.to_sequence |> Sequence.map ~f:(fun l -> (gt, l)))
  |> Sequence.to_list % Sequence.join


let grvs_from_gts_and_vars gts vars =
  rvts_from_gts gts |> fun rvts -> List.cartesian_product rvts (Set.to_list vars)


module TrivialTimeBounds = TrivialTimeBounds.Probabilistic

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


let improve_timebounds twn_state ~conf program scc apprs =
  let open MaybeChanged.Monad in
  let* appr = PlrfBounds.improve_timebounds_plrf program scc (apprs.class_appr, apprs.appr) in
  IntegrateClassicalAnalysis.improve ~twn:twn_state ~mprf_depth:conf.classical_local.run_mprf_depth program
    scc (apprs.class_appr, appr)


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

let improve_scc_classically ~classical_local_conf program program_vars scc_locs apprs =
  ProofOutput.start_new_subproof ();

  let scc_with_in_and_out = Program.scc_gts_from_locs_with_incoming_and_outgoing program scc_locs in
  let overappr_classical_program =
    Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program
  in
  let class_appr =
    coerce_from_classical_approximation apprs.class_appr
    |> ClassicAnalysis.improve_scc ~conf:classical_local_conf scc_locs overappr_classical_program
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


let improve_scc_probabilistically ~conf program program_vars scc_locs apprs : ExpApproximation.t =
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
  let twn_state =
    (ref @@ IntegrateClassicalAnalysis.initial_twn_state program scc, conf.classical_local.twn)
  in
  let improve_scc_ appr =
    let open MaybeChanged.Monad in
    let appr =
      improve_sizebounds program program_vars scc (rvts_scc, grvs_in_and_out) elcbs { apprs with appr }
    in
    let* appr = improve_timebounds twn_state ~conf program scc { apprs with appr } in
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


let improve_scc ~conf program program_vars scc_locs apprs =
  (* Compute trivial time bounds *)
  let apprs =
    improve_scc_classically ~classical_local_conf:conf.classical_local program program_vars scc_locs apprs
  in
  let appr = improve_scc_probabilistically ~conf program program_vars scc_locs apprs in
  { apprs with appr }


let logger_cfr = Logging.(get CFR)

let analyse_refined_scc ~conf apprs_orig program_orig scc_orig program_vars cfr ~refined_program =
  (* The new sccs which do not occur in the original program. *)
  let cfr_sccs_locs =
    let orig_sccs = Program.sccs_locs program_orig in
    Program.sccs_locs refined_program
    |> List.filter ~f:(fun cfr_scc -> not (List.exists ~f:(Set.equal cfr_scc) orig_sccs))
  in

  (* Start a new (global) subproof *)
  ProofOutput.start_new_subproof ();

  (* analyse refined program *)
  ProofOutput.add_to_proof (fun () ->
      FormattedString.mk_str_header_big "Analysing control-flow refined program");
  let updated_apprs_cfr =
    let update apprs scc_locs =
      let execute () = improve_scc ~conf refined_program program_vars scc_locs apprs in
      Logger.with_log logger_cfr Logger.INFO
        (fun () -> ("CFR.apply_single_cfr.improve_scc", [ ("scc_locs", LocationSet.to_string scc_locs) ]))
        execute
    in
    List.fold_left cfr_sccs_locs
      ~init:(CFR.Probabilistic.create_new_apprs program_orig refined_program apprs_orig)
      ~f:update
  in

  (* Check if CFR obtained improvement *)
  let org_bound =
    RationalBound.sum
      (Sequence.map ~f:(ExpApproximation.timebound apprs_orig.appr) (Set.to_sequence scc_orig))
  in
  let cfr_bound =
    let cfr_transitions = Set.diff (Program.gts refined_program) (Program.gts program_orig) in
    Set.to_sequence cfr_transitions
    |> Sequence.map ~f:(ExpApproximation.timebound updated_apprs_cfr.appr)
    |> RationalBound.sum
  in
  CFR.Probabilistic.add_proof_to_global_proof cfr ~refined_program
    ~refined_bound_str:(RationalBound.show_complexity @@ RationalBound.asymptotic_complexity cfr_bound);

  (* Get CFR proof & restore original proof *)
  let cfr_subproof = ProofOutput.get_subproof () in

  if RationalBound.compare_asy org_bound cfr_bound < 1 then (
    Logger.log logger_cfr Logger.INFO (fun () ->
        ( "NOT_IMPROVED",
          [
            ("original bound", RationalBound.to_string ~pretty:true org_bound);
            ("cfr bound", RationalBound.to_string ~pretty:true cfr_bound);
            (CFR.Probabilistic.method_name cfr ^ " bound", RationalBound.to_string ~pretty:true cfr_bound);
          ] ));
    CFRTypes.DontKeepRefinedProgram)
  else
    CFRTypes.KeepRefinedProgram
      ProofOutput.LocalProofOutput.{ result = (refined_program, updated_apprs_cfr); proof = cfr_subproof }


let handle_cfrs ~conf scc_locs program program_vars apprs =
  let scc_gts = Program.scc_gts_from_locs program scc_locs in
  let scc_transitions = Program.scc_transitions_from_locs program scc_locs in
  let non_linear_transitions =
    Set.filter
      ~f:(not % RationalBound.is_linear % ExpApproximation.timebound apprs.appr % Transition.gt)
      scc_transitions
  in
  if Set.is_empty non_linear_transitions then
    (program, apprs)
  else
    let refinement_result =
      CFR.Probabilistic.iter_cfrs program ~scc_orig:scc_transitions
        ~transitions_to_refine:non_linear_transitions
        ~compute_timelimit:(fun () ->
          CFR.Probabilistic.compute_timeout_time program
            ~infinite_timebound:
              (RationalBound.is_infinity % ExpApproximation.timebound apprs.appr % Transition.gt)
            scc_transitions)
        (analyse_refined_scc ~conf apprs program scc_gts program_vars)
        conf.cfrs
    in
    match refinement_result with
    | None -> (program, apprs)
    | Some { result; proof } ->
        ProofOutput.add_local_proof_to_proof proof;
        result


let improve_scc_and_try_cfrs ~conf program program_vars scc_locs apprs =
  improve_scc ~conf program program_vars scc_locs apprs
  (* Apply cfrs if requested *)
  |> handle_cfrs ~conf scc_locs program program_vars


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


let perform_analysis ?(conf = default_configuration) program =
  let program_vars = Program.input_vars program in
  let sccs = Program.sccs_locs program in

  let apprs =
    { appr = ExpApproximation.empty; class_appr = ClassicalApproximation.empty }
    |> TrivialTimeBounds.compute program
  in
  let program, apprs =
    List.fold sccs ~init:(program, apprs) ~f:(fun (program, apprs) scc_locs ->
        improve_scc_and_try_cfrs ~conf program program_vars scc_locs apprs)
  in
  let apprs = lift_appr_to_exp_costbounds program apprs in

  (program, apprs)
