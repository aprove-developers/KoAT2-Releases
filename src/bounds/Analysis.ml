open! OurBase
open Polynomials

(* The types below are used to restrict certain analyses methods to certain underlying types *)

type !'bound goal = Complexity : Bounds.Bound.t goal | Termination : Bounds.BinaryBound.t goal

type (!'prog_modules_t, !'bound) closed_form_size_bounds =
  | NoClosedFormSizeBounds : ('prog_modules_t, 'bound) closed_form_size_bounds
  | ComputeClosedFormSizeBounds : (ProgramModules.program_modules_t, Bounds.Bound.t) closed_form_size_bounds

type (!'prog_modules_t, !'bound) local_configuration = {
  run_mprf_depth : int option;
  twn : bool;
  closed_form_size_bounds : ('prog_modules_t, 'bound) closed_form_size_bounds;
  goal : 'bound goal;
}

type (!'prog_modules_t, !'bound) analysis_configuration = {
  local_configuration : ('prog_modules_t, 'bound) local_configuration;
  cfrs : 'prog_modules_t CFR.cfr_ list;
}

type classical_program_conf_type = (ProgramModules.program_modules_t, Bounds.Bound.t) analysis_configuration
type measure = [ `Cost | `Time ] [@@deriving show]

let logger = Logging.(get Time)
let logger_cfr = Logging.(get CFR)

let default_local_configuration : ('a, Bounds.Bound.t) local_configuration =
  {
    run_mprf_depth = Some 1;
    twn = false;
    goal = Complexity;
    closed_form_size_bounds = NoClosedFormSizeBounds;
  }


let default_configuration : ('a, Bounds.Bound.t) analysis_configuration =
  { cfrs = []; local_configuration = default_local_configuration }


module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)
  module TrivialTimeBounds = TrivialTimeBounds.Make (Bound) (PM)
  module CostBounds = CostBounds.Make (Bound) (PM)
  module LSB = LocalSizeBound.Make (PM.TransitionLabel) (PM.Transition) (PM.Program)
  module MultiphaseRankingFunction = MultiphaseRankingFunction.Make (Bound) (PM)
  module RVG = RVGTypes.MakeRVG (PM)
  module SizeBounds = SizeBounds.Make (PM)
  module TWN = TWN.Make (Bound) (PM)
  module CFR = CFR.CFR (PM) (Bound)

  type allowed_local_conf_type = (PM.program_modules_t, Bound.t) local_configuration
  type appr = Approximation.t

  let add_bound = function
    | `Time -> Approximation.add_timebound
    | `Cost -> Approximation.add_costbound


  let get_bound = function
    | `Time -> Approximation.timebound
    | `Cost -> Approximation.costbound


  (** Checks if a transition is bounded *)
  let bounded measure appr transition =
    match measure with
    | `Time -> Approximation.is_time_bounded appr transition
    | `Cost -> Polynomial.is_const (Transition.cost transition)


  let rec knowledge_propagation (scc : TransitionSet.t) program appr =
    let execute () =
      scc |> Set.to_sequence
      |> MaybeChanged.fold_sequence
           ~f:(fun appr transition ->
             let new_bound =
               Program.pre program transition |> Set.to_sequence
               |> Sequence.map ~f:(Approximation.timebound appr)
               |> Bound.sum
             in
             let original_bound = get_bound `Time appr transition in
             if Bound.compare_asy original_bound new_bound = 1 then (
               ProofOutput.add_str_paragraph_to_proof (fun () ->
                   "knowledge_propagation leads to new time bound "
                   ^ Bound.to_string ~pretty:true new_bound
                   ^ " for transition "
                   ^ Transition.to_string_pretty transition);
               add_bound `Time new_bound transition appr |> MaybeChanged.changed)
             else
               MaybeChanged.same appr)
           ~init:appr
      |> MaybeChanged.if_changed (knowledge_propagation scc program)
      |> MaybeChanged.unpack
    in
    Logger.with_log logger Logger.INFO
      (fun () -> ("knowledge prop. ", [ ("scc", TransitionSet.to_string scc) ]))
      execute


  module UnliftedTimeBound = UnliftedBounds.UnliftedTimeBound.Make (PM) (Bound)

  let improve_with_unlifted_time_bound measure appr unlifted_bound =
    let new_bound, compute_proof =
      UnliftedTimeBound.lift_and_get_proof ~get_sizebound:(Approximation.sizebound appr)
        ~get_timebound:(Approximation.timebound appr) unlifted_bound
    in
    let decr_transitions = UnliftedTimeBound.measure_decr_transitions unlifted_bound in
    let result_appr_mc =
      Set.fold decr_transitions
        ~f:(fun appr_mc t ->
          (* check if bound has improved *)
          if Bound.compare_asy (get_bound measure appr t) new_bound = 1 then
            MaybeChanged.flat_map (MaybeChanged.changed % add_bound measure new_bound t) appr_mc
          else
            appr_mc)
        ~init:(MaybeChanged.same appr)
    in
    if MaybeChanged.has_changed result_appr_mc then
      ProofOutput.add_to_proof_with_format compute_proof;
    result_appr_mc


  let improve_with_rank_mprf measure program appr rank =
    let unlifted_bound = MultiphaseRankingFunction.to_unlifted_bound program rank in
    improve_with_unlifted_time_bound measure appr unlifted_bound


  (* We initially compute all possible twn loops.
     Then we prove termination upon demand and propagate twn loops to unlifted time bounds. *)
  type twn_state = { remaining_twn_loops : TWN.twn_loop ProofOutput.LocalProofOutput.with_proof List.t }

  let initial_twn_state program scc =
    let all_loops = TWN.find_all_possible_loops_for_scc scc program in
    { remaining_twn_loops = all_loops }


  let empty_twn_state = { remaining_twn_loops = [] }

  let improve_with_twn ~(conf : allowed_local_conf_type) program scc twn_state appr =
    let not_all_trans_bounded twn_loop =
      TWN.handled_transitions (ProofOutput.LocalProofOutput.result twn_loop)
      |> Set.exists ~f:(not % Approximation.is_time_bounded appr)
    in
    let remaining_twn_loops, appr_mc =
      List.fold_left !twn_state.remaining_twn_loops
        ~init:([], MaybeChanged.same appr)
        ~f:(fun (remaining_twn_loops, appr_mc) twn_loop ->
          let appr = MaybeChanged.unpack appr_mc in
          let twn_loop_res = ProofOutput.LocalProofOutput.result twn_loop in
          if not_all_trans_bounded twn_loop then
            if
              let heuristic_size_bounds (goal : Bound.t goal) =
                match goal with
                | Complexity -> Approximation.sizebound
                | Termination -> fun _ _ _ -> Bound.one
              in
              TWN.finite_bound_possible_if_terminating ~get_timebound:(Approximation.timebound appr)
                ~get_sizebound:(heuristic_size_bounds conf.goal appr)
                twn_loop_res
            then
              (* compute a new global time bound *)
              let unlifted_bound = TWN.to_unlifted_bounds twn_loop in
              let new_appr_mc =
                MaybeChanged.flat_map
                  (fun appr -> improve_with_unlifted_time_bound `Time appr unlifted_bound)
                  appr_mc
              in
              (remaining_twn_loops, new_appr_mc)
            else
              (* We wouldn't be able to compute a finite global time bound from this TWN Loop for now. So keep loop for later *)
              (twn_loop :: remaining_twn_loops, appr_mc)
          else
            (* all transitions handled by this TWN are already terminating. Get rid of it *)
            (remaining_twn_loops, appr_mc))
    in
    (* update twn_state to updated list of remaining loops *)
    twn_state := { remaining_twn_loops };
    appr_mc


  let local_rank ~(conf : allowed_local_conf_type) (scc : TransitionSet.t) measure program max_depth appr =
    let get_unbounded_vars transition =
      match conf.goal with
      | Termination -> VarSet.empty
      | Complexity ->
          program |> Program.input_vars
          |> Set.filter ~f:(Bound.is_infinity % Approximation.sizebound appr transition)
    in
    let is_time_bounded = Bound.is_finite % Approximation.timebound appr in
    let unbounded_transitions =
      scc
      |> tap (fun scc ->
             Logger.log logger Logger.INFO (fun () ->
                 ("improve_timebound", [ ("scc", TransitionSet.to_string scc) ])))
      |> Set.filter ~f:(not % bounded measure appr)
    in
    let scc_overapprox_nonlinear = TransitionSet.map ~f:Transition.overapprox_nonlinear_updates scc in
    let rankfunc_computation depth =
      let compute_function trans =
        MultiphaseRankingFunction.find_scc measure program is_time_bounded get_unbounded_vars
          scc_overapprox_nonlinear depth
        @@ Option.value_exn
        @@ Set.binary_search scc_overapprox_nonlinear ~compare:Transition.compare `First_equal_to trans
      in
      Set.to_array unbounded_transitions |> Parmap.array_parmap compute_function |> Array.to_sequence
      |> Sequence.filter_opt
    in
    (* Compute ranking functions up to the minimum depth such that at least one ranking functino is found
       * or the depth is max_depth *)
    (* Note that enums are lazy *)
    let rankfuncs =
      Sequence.range ~stop:`inclusive 1 max_depth
      |> Sequence.map ~f:rankfunc_computation
      |> Sequence.hd % Sequence.filter ~f:(not % Sequence.is_empty)
      |? Sequence.empty
    in
    rankfuncs |> MaybeChanged.fold_sequence ~init:appr ~f:(improve_with_rank_mprf measure program)


  let run_local ~(conf : allowed_local_conf_type) (scc : TransitionSet.t) twn_state measure program appr =
    MaybeChanged.(
      return appr >>= fun appr ->
      (match conf.run_mprf_depth with
      | Some max_depth -> local_rank ~conf scc measure program max_depth appr
      | None -> MaybeChanged.return appr)
      >>= fun appr ->
      match (measure, conf.twn) with
      | `Cost, _ -> MaybeChanged.return appr
      | `Time, false -> MaybeChanged.return appr
      | `Time, true -> improve_with_twn ~conf program scc twn_state appr)


  let improve_timebound ~(conf : allowed_local_conf_type) (scc : TransitionSet.t) twn_state measure program
      appr =
    let execute () = run_local ~conf scc twn_state measure program appr in
    Logger.with_log logger Logger.INFO
      (fun () ->
        ("improve_bounds", [ ("scc", TransitionSet.to_string scc); ("measure", show_measure measure) ]))
      execute


  let improve_size_bounds ~(conf : allowed_local_conf_type) program rvg_with_sccs scc lsb_table =
    let twn_size_bounds ~(conf : allowed_local_conf_type) (scc : TransitionSet.t) (program : Program.t)
        (appr : Approximation.t) =
      match conf.closed_form_size_bounds with
      | NoClosedFormSizeBounds -> appr
      | ComputeClosedFormSizeBounds ->
          TWNSizeBounds.improve program ~scc:(Option.some scc) appr
          |> SolvableSizeBounds.improve program ~scc:(Option.some scc)
    in
    match conf.goal with
    | Termination -> identity
    | Complexity ->
        fun (appr : Approximation.t) ->
          SizeBounds.improve program (Lazy.force rvg_with_sccs) (Map.find @@ Lazy.force lsb_table) appr
          |> twn_size_bounds ~conf scc program


  let compute_lsbs program scc_locs =
    Lazy.from_fun (fun () ->
        let input_vars = Program.input_vars program in
        let all_rvs_of_scc_and_out =
          let scc_with_in_and_out =
            Program.scc_transitions_from_locs_with_incoming_and_outgoing program scc_locs
          in
          Sequence.cartesian_product (Set.to_sequence scc_with_in_and_out) (Set.to_sequence input_vars)
        in
        all_rvs_of_scc_and_out
        |> Sequence.filter_map ~f:(fun ((t, v) as rv) ->
               let open OptionMonad in
               let+ lsb = LSB.compute_bound input_vars t v in
               (rv, lsb))
        |> Map.of_sequence_exn (module RV))


  let compute_rvg_with_sccs ~(conf : allowed_local_conf_type) opt_lsbs program scc_locs =
    Lazy.from_fun (fun () ->
        let scc_transitions_with_out =
          Program.scc_transitions_from_locs_with_incoming_and_outgoing program scc_locs
        in
        RVG.rvg_from_transitionset_with_sccs
          (Option.map ~f:(LSB.vars % Tuple2.first) % Map.find (Lazy.force opt_lsbs))
          program scc_transitions_with_out)


  let reset_all_caches () =
    (* TODO: Get rid of implicit caching in the following modules. Write a record to group all explicit caches *)
    TWN.reset_cfr ();
    TWNSizeBounds.reset_cfr ();
    SolvableSizeBounds.reset_cfr ()


  let improve_scc ~(conf : allowed_local_conf_type) scc_locs program appr =
    let lsbs = compute_lsbs program scc_locs in
    let rvg_with_sccs = compute_rvg_with_sccs ~conf lsbs program scc_locs in
    let scc = Program.scc_transitions_from_locs program scc_locs in
    let twn_state =
      if conf.twn then
        ref (initial_twn_state program scc)
      else
        ref empty_twn_state
    in
    let improvement_step appr =
      knowledge_propagation scc program appr
      |> improve_size_bounds ~conf program rvg_with_sccs scc lsbs
      |> improve_timebound ~conf scc twn_state `Time program
    in

    (* reset all caches, since we might prior have analysed a different version of the same SCC (i.e., due to CFR) *)
    reset_all_caches ();

    (* First compute initial size bounds for the SCC and then iterate by computing size and time bounds alteratingly *)
    improve_size_bounds ~conf program rvg_with_sccs scc lsbs appr
    |> knowledge_propagation scc program
    |> MaybeChanged.unpack % improve_timebound ~conf scc twn_state `Time program
    |> Util.find_fixpoint improvement_step
end

module Classical (Bound : BoundType.Bound) = struct
  include Make (Bound) (ProgramModules)
  open ProgramModules

  type allowed_conf_type = (program_modules_t, Bound.t) analysis_configuration

  let scc_cost_bounds ~conf program scc appr =
    if Set.exists ~f:(not % Polynomial.is_const % Transition.cost) scc then
      MaybeChanged.unpack (improve_timebound ~conf scc (ref empty_twn_state) `Cost program appr)
    else
      appr


  let analyse_refined_scc ~(conf : allowed_local_conf_type) appr_orig program_orig scc_orig cfr
      ~refined_program =
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
    let updated_appr_cfr =
      let update appr scc_locs =
        let execute () = improve_scc ~conf scc_locs refined_program appr in
        Logger.with_log logger_cfr Logger.INFO
          (fun () -> ("CFR.apply_single_cfr.improve_scc", [ ("scc_locs", LocationSet.to_string scc_locs) ]))
          execute
      in
      List.fold_left cfr_sccs_locs ~init:(CFR.merge_appr program_orig refined_program appr_orig) ~f:update
    in

    (* Check if CFR obtained improvement *)
    let org_bound =
      Bound.sum (Sequence.map ~f:(Approximation.timebound appr_orig) (Set.to_sequence scc_orig))
    in
    let cfr_bound =
      let cfr_transitions =
        Set.diff (Program.transitions refined_program) (Program.transitions program_orig)
      in
      Set.to_sequence cfr_transitions
      |> Sequence.map ~f:(Approximation.timebound updated_appr_cfr)
      |> Bound.sum
    in
    CFR.add_proof_to_global_proof cfr ~refined_program
      ~refined_bound_str:(Bound.show_complexity @@ Bound.asymptotic_complexity cfr_bound);

    (* Get CFR proof & restore original proof *)
    let cfr_subproof = ProofOutput.get_subproof () in

    if Bound.compare_asy org_bound cfr_bound < 1 then (
      Logger.log logger_cfr Logger.INFO (fun () ->
          ( "NOT_IMPROVED",
            [
              ("original bound", Bound.to_string ~pretty:true org_bound);
              (CFR.method_name cfr ^ " bound", Bound.to_string ~pretty:true cfr_bound);
            ] ));
      CFR.DontKeepRefinedProgram)
    else
      CFR.KeepRefinedProgram
        ProofOutput.LocalProofOutput.{ result = (refined_program, updated_appr_cfr); proof = cfr_subproof }


  let handle_cfrs ~(conf : (ProgramModules.program_modules_t, Bound.t) analysis_configuration)
      (scc : TransitionSet.t) program appr =
    let non_linear_transitions = Set.filter ~f:(not % Bound.is_linear % Approximation.timebound appr) scc in
    if Set.is_empty non_linear_transitions then
      (program, appr)
    else
      let refinement_result =
        CFR.iter_cfrs program ~scc_orig:scc ~transitions_to_refine:non_linear_transitions
          ~compute_timelimit:(fun () ->
            CFR.compute_timeout_time program ~get_timebound:(Approximation.timebound appr) scc)
          (analyse_refined_scc ~conf:conf.local_configuration appr program scc)
          conf.cfrs
      in
      match refinement_result with
      | None -> (program, appr)
      | Some { result; proof } ->
          ProofOutput.add_local_proof_to_proof proof;
          result


  let improve_scc_and_try_cfr ~conf program appr scc_locs =
    let scc = Program.scc_transitions_from_locs program scc_locs in
    improve_scc ~conf:conf.local_configuration scc_locs program appr
    (* Apply CFR if requested; timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
    |> handle_cfrs ~conf scc program
    |> fun (program, appr) -> (program, scc_cost_bounds ~conf:conf.local_configuration program scc appr)


  let improve ?(time_cfr = 180) ~(conf : (program_modules_t, Bound.t) analysis_configuration) ~preprocess
      program (appr : Approximation.t) =
    CFR.time_cfr := float_of_int time_cfr;
    let trivial_appr = TrivialTimeBounds.compute program appr in
    let program, appr =
      program |> Program.sccs_locs
      |> List.fold_left
           ~f:(fun (program, appr) scc_locs ->
             Logger.log logger Logger.INFO (fun () ->
                 ("continue analysis", [ ("scc", LocationSet.to_string scc_locs) ]));
             improve_scc_and_try_cfr ~conf program appr scc_locs)
           ~init:(program, trivial_appr)
    in
    (program, CostBounds.infer_from_timebounds program appr)
end
