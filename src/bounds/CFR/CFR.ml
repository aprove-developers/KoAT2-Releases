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

module CFR (PM : ProgramTypes.ProgramModules) (Bound : BoundType.Bound) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)
  module TrivialTimeBounds = TrivialTimeBounds.Make (Bound) (PM)

  type approximation = Approximation.t
  type cfr = PM.program_modules_t cfr_

  let mk_cfr = mk_cfr
  let method_name = method_name
  let perform_cfr = perform_cfr
  let time_cfr = time_cfr

  let handle_timeout_cfr method_name transitions_to_refine =
    Logger.log logger_cfr Logger.INFO (fun () ->
        ( "TIMEOUT_CFR_" ^ method_name,
          [ ("transitions_to_refine", TransitionSet.to_string transitions_to_refine) ] ))


  type 'a refinement_result =
    | DontKeepRefinedProgram
    | KeepRefinedProgram of 'a ProofOutput.LocalProofOutput.with_proof

  (** Iterate over all CFRs and keep the first result for which the given function returns [KeepRefinedProgram]*)
  let iter_cfrs program ~scc_orig ~transitions_to_refine ~compute_timelimit keep_refinement_with_results cfrs
      =
    (* TODO: Can we ensure that CFR alters just one SCC? *)
    let apply_single_cfr cfr =
      let timelimit = compute_timelimit () in
      let execute () =
        let opt =
          Timeout.timed_run timelimit
            ~action:(fun () -> handle_timeout_cfr (method_name cfr) scc_orig)
            (fun () -> perform_cfr cfr program ~transitions_to_refine)
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
      | [] -> None
      | cfr :: cfrs -> (
          if !time_cfr <= 0. then
            None
          else
            match apply_single_cfr cfr with
            | None -> apply_cfrs cfrs
            | Some refined_program -> (
                let keep_refinement_result =
                  let execute () = keep_refinement_with_results cfr ~refined_program in
                  Logger.with_log logger_cfr Logger.INFO
                    (fun () -> ("CFR.iter_cfrs.apply_cfrs.keep_refinement_with_results", []))
                    ~result:(function
                      | DontKeepRefinedProgram -> "Don't keep refined program"
                      | KeepRefinedProgram _ -> "Keep refined program")
                    execute
                in
                match keep_refinement_result with
                | DontKeepRefinedProgram -> apply_cfrs cfrs
                | KeepRefinedProgram res -> Some res))
    in
    apply_cfrs cfrs


  (* timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
  let compute_timeout_time program ~get_timebound scc =
    if Base.Set.exists ~f:(fun t -> Bound.is_infinity (get_timebound t)) scc then
      0.
    else
      let toplogic_later_trans =
        program |> Program.transitions |> flip Base.Set.diff scc
        |> Base.Set.filter ~f:(fun t -> Bound.is_infinity (get_timebound t))
      in
      !time_cfr
      *. float_of_int (Base.Set.length scc)
      /. float_of_int (Base.Set.length toplogic_later_trans + Base.Set.length scc)


  let merge_appr (program : Program.t) (program_cfr : Program.t) appr =
    let unchanged_trans = Set.inter (Program.transitions program) (Program.transitions program_cfr) in
    let appr_cfr = Approximation.empty |> TrivialTimeBounds.compute program_cfr in
    unchanged_trans
    |> Set.fold
         ~f:(fun appr_cfr trans ->
           let timebound = Approximation.timebound appr trans
           and costbound = Approximation.costbound appr trans in
           appr_cfr
           |> Approximation.add_timebound timebound trans
           |> Approximation.add_costbound costbound trans)
         ~init:appr_cfr


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

let pe_with_IRankFinder =
  let perform_cfr program ~transitions_to_refine =
    PartialEvaluation.apply_cfr transitions_to_refine program
  in
  mk_cfr ~method_name:"PartialEvaluationIRankFinder" perform_cfr


let chaining =
  let perform_chaining program ~transitions_to_refine =
    Preprocessor.lift_to_program (Chaining.transform_graph ~scc:(Option.some transitions_to_refine)) program
  in
  mk_cfr ~method_name:"Chaining" perform_chaining


let pe_native pe_config =
  let perform_cfr program ~transitions_to_refine =
    NativePartialEvaluation.ClassicPartialEvaluation.apply_sub_scc_cfr pe_config transitions_to_refine program
    |> MaybeChanged.changed (* TODO: better solution? *)
  in
  mk_cfr ~method_name:"PartialEvaluationNative" perform_cfr


let pe_native_probabilistic pe_config =
  let perform_cfr program ~transitions_to_refine =
    NativePartialEvaluation.ProbabilisticPartialEvaluation.apply_sub_scc_cfr pe_config transitions_to_refine
      program
    |> MaybeChanged.changed (* TODO: better solution? *)
  in
  mk_cfr ~method_name:"PartialEvaluationNativeProbabilistic" perform_cfr
