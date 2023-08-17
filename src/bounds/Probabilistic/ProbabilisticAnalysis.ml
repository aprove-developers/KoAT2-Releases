open OurBase
open ProbabilisticProgramModules
open Bounds
open Approximation.Probabilistic

type configuration = { compute_refined_plrfs : bool }

let default_configuration = { compute_refined_plrfs = false }
let size_logger = Logging.(get ExpSize)

let rvts_from_gts gts =
  Set.to_sequence gts
  |> Sequence.map ~f:(fun gt ->
         GeneralTransition.targets gt |> Set.to_sequence |> Sequence.map ~f:(fun l -> (gt, l)))
  |> Sequence.to_list % Sequence.join


let grvs_from_gts_and_vars gts vars =
  rvts_from_gts gts |> fun rvts -> List.cartesian_product rvts (Base.Set.to_list vars)


let lift_bounds (program, program_gts) program_vars (class_appr, appr) : ExpApproximation.t =
  let lift_time_bounds appr =
    let gt_timebound gt =
      Set.to_sequence (GeneralTransition.transitions gt)
      |> Sequence.map ~f:(ClassicalApproximation.timebound class_appr)
      |> RealBound.of_intbound % Bound.sum
    in
    Set.to_sequence program_gts
    |> Sequence.fold ~f:(fun appr gt -> ExpApproximation.add_timebound (gt_timebound gt) gt appr) ~init:appr
  in

  let lift_size_bounds appr =
    let rv_sizebound ((gt, l), v) =
      Set.to_sequence (GeneralTransition.transitions gt)
      |> Sequence.map ~f:(fun t -> ClassicalApproximation.sizebound class_appr t v)
      |> RealBound.of_intbound % Bound.sum
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


let improve_timebounds_plrf ~conf program scc (class_appr, appr) : ExpApproximation.t MaybeChanged.t =
  let is_exptime_bounded = ExpApproximation.is_time_bounded appr in
  let unbounded_vars (gt, l) =
    Program.input_vars program
    |> Base.Set.filter ~f:(RealBound.is_infinity % ExpApproximation.sizebound appr (gt, l))
  in
  let find_plrfs refined =
    Set.filter ~f:(not % is_exptime_bounded) scc
    |> Set.to_array
    |> Parmap.array_parmap (Plrf.find_scc ~refined program is_exptime_bounded unbounded_vars scc)
    |> Array.to_sequence |> Sequence.filter_opt
  in
  (if conf.compute_refined_plrfs then
     Sequence.of_list [ false; true ]
   else
     Sequence.of_list [ false ])
  |> Sequence.map ~f:find_plrfs
  |> Sequence.filter ~f:(not % Sequence.is_empty)
  |> fun seq ->
  Sequence.hd seq |? Sequence.empty
  |> MaybeChanged.fold_sequence
       ~f:(fun appr -> PlrfBounds.improve_with_plrf program (class_appr, appr))
       ~init:appr


let improve_timebounds twn_state
    ~(classic_conf : NonProbOverappr.program_modules_t Analysis.analysis_configuration) ~conf
    (program, program_gts) scc (class_appr, appr) =
  improve_timebounds_plrf ~conf program scc (class_appr, appr)
  |> MaybeChanged.flat_map
       (IntegrateClassicalAnalysis.improve
          ~twn:(twn_state, classic_conf.twn_configuration)
          ~mprf_depth:classic_conf.run_mprf_depth (program, program_gts) scc class_appr)


let knowledge_propagation program scc appr_mc : ExpApproximation.t MaybeChanged.t =
  let rec iter appr =
    Set.filter ~f:(not % ExpApproximation.is_time_bounded (MaybeChanged.unpack appr_mc)) scc
    |> Set.to_sequence
    |> fun gtset ->
    MaybeChanged.fold_sequence
      ~f:(fun appr gt ->
        let new_bound =
          Set.to_sequence (Program.pre_gt program gt)
          |> Sequence.map ~f:(ExpApproximation.timebound appr)
          |> RealBound.sum
        in
        if RealBound.is_finite new_bound then (
          ProofOutput.add_str_paragraph_to_proof
            FormattedString.(
              fun () ->
                "knowledge_propagation leads to new time bound "
                ^ RealBound.to_string ~pretty:true new_bound
                ^ " for transition "
                ^ GeneralTransition.to_string_pretty gt);
          MaybeChanged.changed (ExpApproximation.add_timebound new_bound gt appr))
        else
          MaybeChanged.same appr)
      ~init:appr gtset
    |> fun appr_mc ->
    if MaybeChanged.has_changed appr_mc then
      MaybeChanged.(appr_mc >>= iter)
    else
      appr_mc
  in
  MaybeChanged.(appr_mc >>= iter)


module ELCBMap = MakeMapCreators1 (GRV)

(* TODO rvs_in instead of rvs_out  *)
let improve_sizebounds program program_vars scc (rvts_scc, rvs_in) elcbs (class_appr, appr) :
    ExpApproximation.t =
  let trivial_sizebounds appr =
    let trivial_sizebound_for_grv ((gt, l), v) =
      (* TODO *)
      let pre_gt = Program.pre_gt program gt in
      let start_loc = GeneralTransition.src gt in
      let pre_size_exp v =
        Set.to_sequence pre_gt
        |> Sequence.map ~f:(fun pre_gt -> ExpApproximation.sizebound appr (pre_gt, start_loc) v)
        |> RealBound.sum
      in
      let pre_size_classical v =
        Set.to_sequence pre_gt
        |> Sequence.map ~f:(Set.to_sequence % GeneralTransition.transitions_to_target start_loc)
        |> Sequence.join
        |> Sequence.map ~f:(fun t -> ClassicalApproximation.sizebound class_appr t v)
        |> RealBound.of_intbound % Bound.sum
      in
      let var_overapprox =
        if Program.is_initial_gt program gt then
          RealBound.of_var v
        else
          pre_size_exp v
      in
      let elcb = Map.find_exn elcbs ((gt, l), v) in
      let elcb_overapprox =
        if Program.is_initial_gt program gt then
          RealBound.of_var v
        else if RealBound.is_linear elcb then
          RealBound.substitute_f pre_size_exp elcb
        else
          RealBound.substitute_f pre_size_classical elcb
      in
      RealBound.add var_overapprox elcb_overapprox
      |> tap (fun r ->
             Logger.log size_logger Logger.DEBUG (fun () ->
                 ( "trivial_sizebound_for_rv",
                   [
                     ("grv", GRV.to_id_string ((gt, l), v));
                     ("var_overapprox", RealBound.to_string var_overapprox);
                     ("elcb", RealBound.to_string elcb);
                     ("elcb_overapprox", RealBound.to_string elcb_overapprox);
                     ("result", RealBound.to_string r);
                   ] )))
    in
    List.fold
      ~f:(fun appr (rvt, v) -> ExpApproximation.add_sizebound (trivial_sizebound_for_grv (rvt, v)) rvt v appr)
      ~init:appr rvs_in
  in

  let nontrivial_sizebounds appr =
    let entry_rvts = Sequence.to_list (BoundsHelper.entry_gts_with_locs program scc) in
    let overappr_var_in gt v =
      GeneralTransition.transitions gt
      |> Program.pre program % Base.Set.choose_exn
      |> Sequence.map ~f:(fun t -> ClassicalApproximation.sizebound class_appr t v) % Base.Set.to_sequence
      |> RealBound.of_intbound % Bound.sum
    in
    let nontrivial_sizebound appr v =
      let execute () =
        let start_value =
          Sequence.of_list entry_rvts
          |> Sequence.map ~f:(fun rvt -> ExpApproximation.sizebound appr rvt v)
          |> RealBound.sum
          |> tap (fun s ->
                 Logger.log size_logger Logger.DEBUG (fun () ->
                     ("start_value", [ ("res", RealBound.to_string s) ])))
        in
        let acc_change_grv (gt, l) =
          let elcb = Map.find_exn elcbs ((gt, l), v) in
          let change_bound = RealBound.substitute_f (overappr_var_in gt) elcb in
          RealBound.(change_bound * ExpApproximation.timebound appr gt)
          |> tap (fun r ->
                 Logger.log size_logger Logger.DEBUG (fun () ->
                     ( "acc_change_rvt",
                       [
                         ("rv", GRV.to_id_string ((gt, l), v));
                         ("elcb", RealBound.to_string elcb);
                         ("res", RealBound.to_string r);
                         ("change_bound", RealBound.to_string change_bound);
                         ("time_bound", RealBound.to_string (ExpApproximation.timebound appr gt));
                       ] )))
        in
        let acc_change = Sequence.of_list rvts_scc |> Sequence.map ~f:acc_change_grv |> RealBound.sum in
        RealBound.(start_value + acc_change)
      in
      Logger.with_log size_logger Logger.DEBUG
        (fun () ->
          ("nontrivial_sizebound", [ ("scc", GeneralTransitionSet.to_id_string scc); ("v", Var.to_string v) ]))
        ~result:RealBound.to_string execute
    in
    Base.Set.fold
      ~f:(fun appr v ->
        let new_bound = nontrivial_sizebound appr v in
        ExpApproximation.add_sizebounds new_bound (List.map ~f:(fun rvt -> (rvt, v)) rvts_scc) appr)
      program_vars ~init:appr
  in

  (* propagate sizes through identity updates *)
  let rec propagate_sizes (appr : ExpApproximation.t) =
    let appr = MaybeChanged.same appr in
    let rvs_scc = List.cartesian_product rvts_scc (Set.to_list program_vars) in
    let all_rvs = List.append rvs_in rvs_scc in
    List.fold_left all_rvs ~init:appr ~f:(fun appr ((gt, l), v) ->
        Set.fold (GeneralTransition.targets gt) ~init:appr ~f:(fun appr_mc target_loc ->
            let appr = MaybeChanged.unpack appr_mc in
            let transitions = Set.to_list (GeneralTransition.transitions_to_target target_loc gt) in
            let transition_updates =
              List.map transitions ~f:(fun (_, t, _) -> TransitionLabel.update t v)
              |> List.map ~f:(Option.bind ~f:UpdateElement.to_polynomial)
              |> OptionMonad.sequence
            in
            (* Propagate bounds if all transitions have linear non-probabilistic updates *)
            match transition_updates with
            | Some update_polys when List.for_all update_polys ~f:Polynomials.Polynomial.is_linear ->
                let pre_sizebound =
                  if Program.is_initial_gt program gt then
                    fun v -> RealBound.of_var v
                  else
                    let pre_gts = Set.to_sequence (Program.pre_gt program gt) in
                    fun v ->
                      Sequence.map pre_gts ~f:(fun pre_gt ->
                          ExpApproximation.sizebound appr (pre_gt, GeneralTransition.src gt) v)
                      |> RealBound.sum
                in
                let propagated_bound =
                  Sequence.of_list update_polys
                  |> Sequence.map ~f:(fun update_poly ->
                         RealBound.substitute_f pre_sizebound (RealBound.of_intpoly update_poly))
                  |> RealBound.sum
                in
                if
                  RealBound.compare_asy propagated_bound (ExpApproximation.sizebound appr (gt, target_loc) v)
                  < 0
                then (
                  Logger.log size_logger Logger.DEBUG (fun () ->
                      ( "propagate_sizes",
                        [
                          ("grv", GRV.to_id_string ((gt, target_loc), v));
                          ("new_bound", RealBound.to_string propagated_bound);
                        ] ));
                  MaybeChanged.flat_map
                    (MaybeChanged.changed % ExpApproximation.add_sizebound propagated_bound (gt, target_loc) v)
                    appr_mc)
                else
                  appr_mc
            | _ -> appr_mc))
    |> MaybeChanged.unpack % MaybeChanged.if_changed propagate_sizes
  in

  trivial_sizebounds appr |> nontrivial_sizebounds |> propagate_sizes


let improve_scc ~(classic_conf : NonProbOverappr.program_modules_t Analysis.analysis_configuration) ~conf
    (program, program_gts) program_vars scc (class_appr, appr) : ExpApproximation.t =
  let rvts_scc = rvts_from_gts scc in
  let rvs_in =
    List.cartesian_product
      (Sequence.to_list @@ BoundsHelper.entry_gts_with_locs program scc)
      (Base.Set.to_list program_vars)
  in
  let elcbs =
    List.cartesian_product rvts_scc (Base.Set.to_list program_vars)
    |> List.append rvs_in
    |> Sequence.map ~f:(fun rv -> (rv, ExpectedLocalChangeBound.compute_elcb program_vars rv))
       % Sequence.of_list
    |> ELCBMap.of_sequence_exn
  in
  let twn_state =
    Option.value_map
      Analysis.(classic_conf.twn_configuration)
      ~default:(ref IntegrateClassicalAnalysis.empty_twn_state)
      ~f:(fun twn_conf -> ref @@ IntegrateClassicalAnalysis.initial_twn_state twn_conf program scc)
  in
  let rec improve_scc_ appr =
    improve_sizebounds program program_vars scc (rvts_scc, rvs_in) elcbs (class_appr, appr) |> fun appr ->
    improve_timebounds twn_state ~classic_conf ~conf (program, program_gts) scc (class_appr, appr)
    |> knowledge_propagation program scc
    |> MaybeChanged.map (fun appr ->
           improve_sizebounds program program_vars scc (rvts_scc, rvs_in) elcbs (class_appr, appr))
    |> fun mc ->
    if MaybeChanged.has_changed mc then
      improve_scc_ (MaybeChanged.unpack mc)
    else
      MaybeChanged.unpack mc
  in
  improve_scc_ appr


let perform_analysis ?(classic_conf = Analysis.default_configuration) ?(conf = default_configuration) program
    class_appr : ExpApproximation.t =
  let program_gts = Program.gts program in
  let program_vars = Program.input_vars program in
  let sccs = Program.sccs_gts program in

  lift_bounds (program, program_gts) program_vars (class_appr, ExpApproximation.empty) |> fun appr ->
  List.fold
    ~f:(fun appr scc_with_locs ->
      improve_scc ~classic_conf ~conf (program, program_gts) program_vars scc_with_locs (class_appr, appr))
    ~init:appr sccs


module ClassicAnalysis = Analysis.Make (NonProbOverappr)

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
