open Batteries
open Polynomials

type cfr_method = Chaining | PartialEvaluationIRankFinder | PartialEvaluationNative of bool * int * bool

(* The types below are used to restrict certain analyses methods to certain underlying types *)
type !'prog_modules_t cfr_configuration =
  | NoCFR : 'prog_modules_t cfr_configuration
  | PerformCFR : cfr_method list -> ProgramModules.program_modules_t cfr_configuration

type !'bound goal = Complexity : Bounds.Bound.t goal | Termination : Bounds.BinaryBound.t goal

type (!'prog_modules_t, 'bound) closed_form_size_bounds =
  | NoClosedFormSizeBounds : ('prog_modules_t, 'bound) closed_form_size_bounds
  | ComputeClosedFormSizeBounds : (ProgramModules.program_modules_t, Bounds.Bound.t) closed_form_size_bounds

type (!'prog_modules_t, 'bound) analysis_configuration = {
  run_mprf_depth : int option;
  twn : bool;
  cfr_configuration : 'prog_modules_t cfr_configuration;
  goal : 'bound goal;
  closed_form_size_bounds : ('prog_modules_t, 'bound) closed_form_size_bounds;
}

type classical_program_conf_type = (ProgramModules.program_modules_t, Bounds.Bound.t) analysis_configuration
type measure = [ `Cost | `Time ] [@@deriving show]

let logger = Logging.(get Time)
let logger_cfr = Logging.(get CFR)

let default_configuration : ('a, Bounds.Bound.t) analysis_configuration =
  {
    run_mprf_depth = Some 1;
    twn = false;
    cfr_configuration = NoCFR;
    goal = Complexity;
    closed_form_size_bounds = NoClosedFormSizeBounds;
  }


module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)
  module TrivialTimeBounds = TrivialTimeBounds.Make (Bound) (PM)
  module CostBounds = CostBounds.Make (Bound) (PM)
  module LSB = LocalSizeBound.Make (PM.TransitionLabel) (PM.Transition) (PM.Program)
  module LSB_Table = Hashtbl.Make (PM.RV)
  module MultiphaseRankingFunction = MultiphaseRankingFunction.Make (Bound) (PM)
  module PENative = NativePartialEvaluation.ClassicPartialEvaluation
  module RVG = RVGTypes.MakeRVG (PM)
  module SizeBounds = SizeBounds.Make (PM)
  module TWN = TWN.Make (Bound) (PM)
  module CFR = CFR.CFR (Bound)
  module PartialEvaluation = PartialEvaluation.Make (Bound)

  type allowed_conf_type = (PM.program_modules_t, Bound.t) analysis_configuration

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
      scc |> Base.Set.to_sequence
      |> MaybeChanged.fold_sequence
           ~f:(fun appr transition ->
             let new_bound =
               Program.pre program transition |> Base.Set.to_sequence
               |> Base.Sequence.map ~f:(Approximation.timebound appr)
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
      OurBase.Set.fold decr_transitions
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

  let improve_with_twn ~(conf : allowed_conf_type) program scc twn_state appr =
    let open! OurBase in
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


  let local_rank ~(conf : allowed_conf_type) (scc : TransitionSet.t) measure program max_depth appr =
    let open! OurBase in
    let get_unbounded_vars transition =
      match conf.goal with
      | Termination -> VarSet.empty
      | Complexity ->
          program |> Program.input_vars
          |> Base.Set.filter ~f:(Bound.is_infinity % Approximation.sizebound appr transition)
    in
    let is_time_bounded = Bound.is_finite % Approximation.timebound appr in
    let unbounded_transitions =
      scc
      |> tap (fun scc ->
             Logger.log logger Logger.INFO (fun () ->
                 ("improve_timebound", [ ("scc", TransitionSet.to_string scc) ])))
      |> Base.Set.filter ~f:(not % bounded measure appr)
    in
    let scc_overapprox_nonlinear = TransitionSet.map ~f:Transition.overapprox_nonlinear_updates scc in
    let rankfunc_computation depth =
      let compute_function trans =
        MultiphaseRankingFunction.find_scc measure program is_time_bounded get_unbounded_vars
          scc_overapprox_nonlinear depth
        @@ Option.value_exn
        @@ Base.Set.binary_search scc_overapprox_nonlinear ~compare:Transition.compare `First_equal_to trans
      in
      Base.Set.to_array unbounded_transitions
      |> Parmap.array_parmap compute_function |> Array.to_sequence |> Sequence.filter_opt
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


  let run_local ~(conf : allowed_conf_type) (scc : TransitionSet.t) twn_state measure program appr =
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


  let improve_timebound ~(conf : allowed_conf_type) (scc : TransitionSet.t) twn_state measure program appr =
    let execute () = run_local ~conf scc twn_state measure program appr in
    Logger.with_log logger Logger.INFO
      (fun () ->
        ("improve_bounds", [ ("scc", TransitionSet.to_string scc); ("measure", show_measure measure) ]))
      execute


  let improve_size_bounds ~(conf : allowed_conf_type) program opt_rvg_with_sccs scc opt_lsb_table =
    let twn_size_bounds ~(conf : allowed_conf_type) (scc : TransitionSet.t) (program : Program.t)
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
          SizeBounds.improve program (Option.get opt_rvg_with_sccs) ~scc:(Option.some scc)
            (LSB_Table.find @@ Option.get opt_lsb_table)
            appr
          |> twn_size_bounds ~conf scc program


  let improve_scc ~(conf : allowed_conf_type) opt_rvg_with_sccs (scc : TransitionSet.t) program opt_lsb_table
      appr =
    let twn_state =
      if conf.twn then
        ref (initial_twn_state program scc)
      else
        ref empty_twn_state
    in
    let improvement appr =
      knowledge_propagation scc program appr
      |> improve_size_bounds ~conf program opt_rvg_with_sccs scc opt_lsb_table
      |> improve_timebound ~conf scc twn_state `Time program
    in
    (* First compute initial time bounds for the SCC and then iterate by computing size and time bounds alteratingly *)
    knowledge_propagation scc program appr
    |> MaybeChanged.unpack % improve_timebound ~conf scc twn_state `Time program
    |> Util.find_fixpoint improvement


  let scc_cost_bounds ~conf program scc appr =
    if Base.Set.exists ~f:(not % Polynomial.is_const % Transition.cost) scc then
      MaybeChanged.unpack (improve_timebound ~conf scc (ref empty_twn_state) `Cost program appr)
    else
      appr


  let reset_all_caches () =
    TWN.reset_cfr ();
    TWNSizeBounds.reset_cfr ();
    SolvableSizeBounds.reset_cfr ()


  let handle_timeout_cfr method_name non_linear_transitions =
    reset_all_caches ();
    Logger.log logger_cfr Logger.INFO (fun () ->
        ("TIMEOUT_CFR_" ^ method_name, [ ("scc", TransitionSet.to_string non_linear_transitions) ]))


  let all_rvs program input_vars =
    List.cartesian_product (Base.Set.to_list (Program.transitions program)) (Base.Set.to_list input_vars)


  let add_missing_lsbs program lsbs =
    let input_vars = program |> Program.input_vars in
    all_rvs program input_vars
    |> List.iter (fun (t, v) ->
           if LSB_Table.mem lsbs (t, v) then
             ()
           else
             LSB_Table.add lsbs (t, v) (LSB.compute_bound input_vars t v))


  let compute_lsbs program =
    let vars = program |> Program.input_vars in
    List.enum (all_rvs program vars)
    |> Enum.map (fun (t, v) -> ((t, v), LSB.compute_bound vars t v))
    |> LSB_Table.of_enum


  let handle_cfr ~(conf : allowed_conf_type) ~(preprocess : Program.t -> Program.t) (scc : TransitionSet.t)
      program opt_rvg opt_lsbs appr : Program.t * Approximation.t * (RVG.t * RVG.scc list lazy_t) option =
    match conf.cfr_configuration with
    | NoCFR -> (program, appr, opt_rvg)
    | PerformCFR cfr ->
        (let module Approximation = Approximation in
        let lsbs = opt_lsbs |? LSB_Table.create 0 in
        let rvg = opt_rvg |? (RVG.empty, Lazy.from_fun (const [])) in
        let apply_cfr (method_name : string) (f_cfr : Program.t -> Program.t MaybeChanged.t)
            (f_proof : Program.t -> string -> unit) (rvg_with_sccs : RVG.t * RVG.scc list Lazy.t)
            (time : float) (non_linear_transitions : TransitionSet.t) ~(preprocess : Program.t -> Program.t)
            (program : Program.t) (appr : Approximation.t) =
          if Base.Set.is_empty non_linear_transitions then
            (program, appr, rvg_with_sccs)
          else
            let mc =
              Logger.log logger_cfr Logger.INFO (fun () ->
                  ( "Analysis_apply_" ^ method_name,
                    [
                      ("non-linear trans", TransitionSet.to_string non_linear_transitions);
                      ("time", string_of_float time);
                    ] ));
              f_cfr program
            in
            if not @@ MaybeChanged.has_changed mc then
              (program, appr, rvg_with_sccs)
            else (
              ProofOutput.add_to_proof (fun () ->
                  FormattedString.mk_str_header_big "Analysing control-flow refined program");
              let program_cfr = mc |> MaybeChanged.unpack |> preprocess in
              (match conf.goal with
              | Complexity -> add_missing_lsbs program_cfr lsbs
              | _ -> ());
              Logger.log logger_cfr Logger.DEBUG (fun () -> ("apply_" ^ method_name, []));
              reset_all_caches ();
              let rvg_with_sccs_cfr =
                match conf.goal with
                | Termination -> (RVG.empty, Lazy.from_fun (const []))
                | Complexity ->
                    RVG.rvg_with_sccs (Option.map (LSB.vars % Tuple2.first) % LSB_Table.find lsbs) program_cfr
              in
              (* The new sccs which do not occur in the original program. *)
              let cfr_sccs =
                Program.sccs program_cfr
                |> List.filter (fun cfr_scc ->
                       not
                         (Base.List.exists
                            ~f:(fun scc_ -> Base.Set.equal cfr_scc scc_)
                            (Program.sccs program)))
              in
              let update_appr appr scc =
                if Base.Set.exists ~f:(Bound.is_infinity % Approximation.timebound appr) scc then (
                  Logger.log logger Logger.INFO (fun () ->
                      (method_name ^ "analysis", [ ("scc", TransitionSet.to_id_string scc) ]));
                  improve_size_bounds ~conf program_cfr (Some rvg_with_sccs_cfr) scc opt_lsbs appr
                  |> improve_scc ~conf (Some rvg_with_sccs_cfr) scc program_cfr opt_lsbs)
                else
                  appr
              in
              let updated_appr_cfr =
                cfr_sccs |> List.fold_left update_appr (CFR.merge_appr program program_cfr appr)
              in
              let handle_no_improvement org_bound cfr_bound =
                ProofOutput.add_to_proof (fun () ->
                    FormattedString.mk_str_header_big "CFR did not improve the program. Rolling back");
                reset_all_caches ();
                Logger.log logger_cfr Logger.INFO (fun () ->
                    ("NOT_IMPROVED", [ ("original bound", org_bound); (method_name ^ " bound", cfr_bound) ]));
                (program, appr, rvg_with_sccs)
              in
              (*Calculates concrete bounds*)
              let handle_cfr () =
                let org_bound =
                  Bound.sum (Base.Sequence.map ~f:(Approximation.timebound appr) (Base.Set.to_sequence scc))
                in
                let cfr_bound =
                  Bound.sum
                    (OurBase.Sequence.map
                       ~f:(fun scc ->
                         Bound.sum
                           (Base.Sequence.map
                              ~f:(Approximation.timebound updated_appr_cfr)
                              (Base.Set.to_sequence scc)))
                       (OurBase.Sequence.of_list cfr_sccs))
                in
                if Bound.compare_asy org_bound cfr_bound < 1 then
                  handle_no_improvement
                    (Bound.to_string ~pretty:true org_bound)
                    (Bound.show_complexity @@ Bound.asymptotic_complexity cfr_bound)
                else (
                  f_proof program_cfr @@ Bound.to_string ~pretty:true cfr_bound;
                  (program_cfr, updated_appr_cfr, rvg_with_sccs_cfr))
              in
              handle_cfr ())
        in
        let compute_non_linear_transitions program appr =
          Base.Set.filter
            ~f:(not % Bound.is_linear % Approximation.timebound appr)
            (Base.Set.inter scc (Program.transitions program))
        in
        let partial_evaluation (program, appr, rvg) =
          let program, appr, rvg =
            if List.mem PartialEvaluationIRankFinder cfr then
              let non_linear_transitions =
                (* TODO: why? *)
                compute_non_linear_transitions program appr
              in
              if !PartialEvaluation.time_cfr > 0. && not (Base.Set.is_empty non_linear_transitions) then
                let time = PartialEvaluation.compute_timeout_time program appr scc in
                let opt =
                  Timeout.timed_run time
                    ~action:(fun () -> handle_timeout_cfr "partial_evaluation" scc)
                    (fun () ->
                      apply_cfr "partial_evaluation"
                        (PartialEvaluation.apply_cfr non_linear_transitions)
                        PartialEvaluation.add_to_proof rvg time non_linear_transitions ~preprocess program
                        appr)
                in
                match opt with
                | Some (res, time_used) ->
                    PartialEvaluation.time_cfr := !PartialEvaluation.time_cfr -. time_used;
                    res
                | None -> (program, appr, rvg)
              else
                (program, appr, rvg)
            else
              (program, appr, rvg)
          in

          let program, appr, rvg =
            match
              List.find_opt
                (function
                  | PartialEvaluationNative _ -> true
                  | Chaining -> false
                  | PartialEvaluationIRankFinder -> false)
                cfr
            with
            | Some (PartialEvaluationNative (fvs, k_encounters, update_invariants)) -> (
                let non_linear_transitions = compute_non_linear_transitions program appr in
                if Base.Set.is_empty non_linear_transitions then
                  (program, appr, rvg)
                else
                  let time = PartialEvaluation.compute_timeout_time program appr scc in
                  let pe_config =
                    NativePartialEvaluation.
                      {
                        abstract =
                          (if fvs then
                             `FVS
                           else
                             `LoopHeads);
                        k_encounters;
                        update_invariants;
                      }
                  in
                  let opt =
                    Timeout.timed_run time
                      ~action:(fun () -> handle_timeout_cfr "partial_evaluation" scc)
                      (fun () ->
                        apply_cfr "partial_evaluation_native"
                          (MaybeChanged.changed % PENative.apply_sub_scc_cfr pe_config non_linear_transitions)
                          PartialEvaluation.add_to_proof rvg time non_linear_transitions ~preprocess program
                          appr)
                  in
                  match opt with
                  | Some (res, time_used) ->
                      PartialEvaluation.time_cfr := !PartialEvaluation.time_cfr -. time_used;
                      res
                  | None -> (program, appr, rvg))
            | _ -> (program, appr, rvg)
          in
          (program, appr, rvg)
        in
        let non_linear_transitions = compute_non_linear_transitions program appr in
        let chaining () =
          Timeout.timed_run 10.
            ~action:(fun () -> handle_timeout_cfr "partial_evaluation" scc)
            (fun () ->
              apply_cfr "chaining"
                (CFR.lift_to_program (Chaining.transform_graph ~scc:(Option.some scc)))
                (fun _ _ -> ())
                rvg 10. non_linear_transitions ~preprocess program appr)
          |> Option.map_default Tuple2.first (program, appr, rvg)
        in
        (if (not (Base.Set.is_empty non_linear_transitions)) && List.mem Chaining cfr then
           chaining ()
         else
           (program, appr, rvg))
        |>
        if
          !PartialEvaluation.time_cfr > 0.
          && (not (Base.Set.is_empty non_linear_transitions))
          && List.mem PartialEvaluationIRankFinder cfr
        then
          partial_evaluation
        else
          identity)
        |> Tuple3.map3 Option.some


  let improve ?(time_cfr = 180) ~(conf : allowed_conf_type) ~preprocess program appr =
    PartialEvaluation.time_cfr := float_of_int time_cfr;
    let trivial_appr = TrivialTimeBounds.compute program appr in
    let opt_lsbs =
      match conf.goal with
      | Termination -> None
      | Complexity -> Option.some @@ compute_lsbs program
    in
    let opt_rvg =
      match conf.goal with
      | Termination -> None
      | Complexity ->
          Option.some
          @@ RVG.rvg_with_sccs
               (Option.map (LSB.vars % Tuple2.first) % LSB_Table.find (Option.get opt_lsbs))
               program
    in
    let program, appr =
      program |> Program.sccs
      |> List.fold_left
           (fun (program, appr, opt_rvg) scc_orig ->
             let improve_scc scc =
               if Base.Set.exists ~f:(fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc then
                 appr
                 |> tap
                      (const
                      @@ Logger.log logger Logger.INFO (fun () ->
                             ("continue analysis", [ ("scc", TransitionSet.to_id_string scc) ])))
                 |> improve_size_bounds ~conf program opt_rvg scc opt_lsbs
                 |> improve_scc ~conf opt_rvg scc program opt_lsbs
                 (* Apply CFR if requested; timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
                 |> handle_cfr ~conf ~preprocess scc program opt_rvg opt_lsbs
                 |> fun (program, appr, opt_rvg) -> (program, scc_cost_bounds ~conf program scc appr, opt_rvg)
               else
                 (program, appr, opt_rvg)
             in
             (* Check if SCC still exists and keep only existing transitions (Preprocessing in cfr might otherwise cut them ) *)
             match conf.cfr_configuration with
             | NoCFR -> improve_scc scc_orig
             | PerformCFR _ ->
                 let scc = Base.Set.inter scc_orig (Program.transitions program) in
                 if Base.Set.is_empty scc then
                   (program, appr, opt_rvg)
                 else
                   improve_scc scc)
           (program, trivial_appr, opt_rvg)
      |> Tuple3.get12
    in
    (program, CostBounds.infer_from_timebounds program appr)
end
