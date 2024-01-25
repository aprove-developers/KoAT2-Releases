open! OurBase

type ('trans_lbl, 'trans_lbl_cmp_wit) _trans_set =
  (Location.t * 'trans_lbl * Location.t, 'trans_lbl_cmp_wit TransitionComparator.comparator_witness) Set.t

type 'prog_modules_t cfr_ =
  | Cfr : {
      method_name : String.t;
      perform_cfr :
        ('trans_lbl, 'trans_lbl_cmp_wit, 'trans_graph) GenericProgram_.t ->
        transitions_to_refine:('trans_lbl, 'trans_lbl_cmp_wit) _trans_set ->
        ('trans_lbl, 'trans_lbl_cmp_wit, 'trans_graph) GenericProgram_.t MaybeChanged.t;
    }
      -> ('trans_lbl * 'trans_lbl_cmp_wit * 'trans_graph) ProgramTypes.program_modules_meta cfr_

let mk_cfr ~method_name perform_cfr = Cfr { method_name; perform_cfr }

let method_name = function
  | Cfr cfr -> cfr.method_name


let perform_cfr = function
  | Cfr cfr -> cfr.perform_cfr


let logger_cfr = Logging.(get CFR)

(* Timeouts *)
(* Measures the time spend on CFR. *)
let time_cfr = ref 180.

module Make (PM : ProgramTypes.ProgramModules) (Bound : BoundType.Bound) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)

  type program = Program.t
  type transition = Transition.t
  type transition_set = TransitionSet.t
  type cfr = PM.program_modules_t cfr_

  let mk_cfr = mk_cfr
  let method_name = method_name
  let perform_cfr = perform_cfr
  let time_cfr = time_cfr

  let handle_timeout_cfr method_name transitions_to_refine =
    Logger.log logger_cfr Logger.INFO (fun () ->
        ( "TIMEOUT_CFR_" ^ method_name,
          [ ("transitions_to_refine", TransitionSet.to_string transitions_to_refine) ] ))


  let refine_and_analyse ~preprocess cfr program ~transitions_to_refine keep_refinement_with_results =
    let open MaybeChanged.Monad in
    let+ refined_program = perform_cfr cfr program ~transitions_to_refine |> MaybeChanged.map preprocess in
    let execute () = keep_refinement_with_results cfr ~refined_program in
    Logger.with_log logger_cfr Logger.INFO
      (fun () -> ("CFR.iter_cfrs.apply_cfrs.keep_refinement_with_results", []))
      ~result:(function
        | CFRTypes.DontKeepRefinedProgram -> "Don't keep refined program"
        | KeepRefinedProgram _ -> "Keep refined program")
      execute


  (** Iterate over all CFRs and keep the first result for which the given function returns [KeepRefinedProgram]*)
  let iter_cfrs ?(preprocess = identity) program ~scc_orig ~transitions_to_refine ~compute_timelimit
      keep_refinement_with_results cfrs =
    (* TODO: Can we ensure that CFR alters just one SCC? *)
    let apply_single_cfr cfr =
      let timelimit = compute_timelimit () in
      let execute () =
        let opt =
          Timeout.timed_run timelimit
            ~action:(fun () -> handle_timeout_cfr (method_name cfr) scc_orig)
            (fun () ->
              refine_and_analyse ~preprocess cfr program ~transitions_to_refine keep_refinement_with_results)
        in
        match opt with
        | None -> None
        | Some (res_mc, time_used) ->
            time_cfr := !time_cfr -. time_used;
            Option.some_if (MaybeChanged.has_changed res_mc) (MaybeChanged.unpack res_mc)
      in
      Logger.with_log logger_cfr Logger.INFO
        (fun () ->
          ( "CFR.iter_cfrs.apply_single_cfr",
            [
              ("method", method_name cfr);
              ("transition_to_refine", TransitionSet.to_id_string transitions_to_refine);
              ("timelimit", string_of_float timelimit);
            ] ))
        execute
        ~result:(Printf.sprintf "obtained refined program: %b" % Option.is_some)
    in
    let rec apply_cfrs = function
      | cfr :: cfrs when !time_cfr > 0. -> (
          match apply_single_cfr cfr with
          | None -> apply_cfrs cfrs
          | Some CFRTypes.DontKeepRefinedProgram -> apply_cfrs cfrs
          | Some (KeepRefinedProgram res) -> Some res)
      | _ -> None
    in
    apply_cfrs cfrs


  (* timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
  let compute_timeout_time program ~infinite_timebound scc =
    if Base.Set.exists ~f:(fun t -> infinite_timebound t) scc then
      0.
    else
      let toplogic_later_trans =
        program |> Program.transitions |> flip Base.Set.diff scc
        |> Base.Set.filter ~f:(fun t -> infinite_timebound t)
      in
      !time_cfr
      *. float_of_int (Base.Set.length scc)
      /. float_of_int (Base.Set.length toplogic_later_trans + Base.Set.length scc)


  let add_proof_to_global_proof cfr ~refined_program ~refined_bound_str =
    ProofOutput.add_to_proof_with_format
    @@ FormattedString.(
         fun format ->
           mk_header_small (mk_str "CFR: Improvement to new bound with the following program: ")
           <> mk_paragraph
                (mk_str ("method: " ^ method_name cfr ^ " new bound: ")
                <> mk_newline
                <> mk_paragraph (mk_str refined_bound_str)
                <> mk_str "cfr-program: " <> mk_newline
                <> mk_paragraph (Program.to_formatted_string ~pretty:true refined_program))
           <>
           match format with
           | Formatter.Html ->
               let module GP = GraphPrint.MakeForClassicalAnalysis (PM) in
               mk_raw_str GP.(print_system_pretty_html refined_program)
           | _ -> FormattedString.Empty)
end

module Classical (Bound : BoundType.Bound) = struct
  open ProgramModules
  include Make (ProgramModules) (Bound)
  module TrivialTimeBounds = TrivialTimeBounds.Classical (Bound)

  type appr = Approximation.t

  let create_new_appr program program_cfr appr =
    let common_trans = Set.inter (Program.transitions program) (Program.transitions program_cfr) in
    Approximation.filter_transitions_and_rvs (Set.mem common_trans)
      (Set.mem common_trans % RV.transition)
      appr
    |> TrivialTimeBounds.compute program_cfr
end

module Probabilistic = struct
  open ProbabilisticProgramModules
  open Approximation.Probabilistic

  let create_new_appr_classical program program_cfr class_appr =
    let common_trans = Set.inter (Program.transitions program) (Program.transitions program_cfr) in
    ClassicalApproximation.filter_transitions_and_rvs (Set.mem common_trans)
      (Set.mem common_trans % RV.transition)
      class_appr


  let create_new_appr program program_cfr appr =
    let common_trans = Set.inter (Program.gts program) (Program.gts program_cfr) in
    ExpApproximation.filter_transitions_and_rvs (Set.mem common_trans) (Set.mem common_trans % GRV.gt) appr


  let create_new_apprs program program_cfr apprs =
    TrivialTimeBounds.Probabilistic.compute program_cfr
      {
        class_appr = create_new_appr_classical program program_cfr apprs.class_appr;
        appr = create_new_appr program program_cfr apprs.appr;
      }


  include Make (ProbabilisticProgramModules) (Bounds.RationalBound)
end

let chaining =
  let perform_chaining program ~transitions_to_refine =
    Preprocessor.lift_to_program (Chaining.transform_graph ~scc:(Option.some transitions_to_refine)) program
  in
  mk_cfr ~method_name:"Chaining" perform_chaining


let pe pe_config =
  let perform_cfr program ~transitions_to_refine =
    PartialEvaluation.ClassicPartialEvaluation.apply_sub_scc_cfr pe_config transitions_to_refine program
    |> MaybeChanged.changed (* TODO: better solution? *)
  in
  mk_cfr ~method_name:"PartialEvaluation" perform_cfr


let pe_probabilistic pe_config =
  let perform_cfr program ~transitions_to_refine =
    PartialEvaluation.ProbabilisticPartialEvaluation.apply_sub_scc_cfr pe_config transitions_to_refine program
    |> MaybeChanged.changed (* TODO: better solution? *)
  in
  mk_cfr ~method_name:"PartialEvaluationProbabilistic" perform_cfr
