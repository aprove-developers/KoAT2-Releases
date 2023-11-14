open! OurBase

type ('trans_lbl, 'trans_lbl_cmp_wit) _trans_set =
  (Location.t * 'trans_lbl * Location.t, 'trans_lbl_cmp_wit TransitionComparator.comparator_witness) Set.t

type 'prog_modules_t cfr_ =
  | Cfr : {
      method_name : String.t;
      perform_cfr :
        ('trans_lbl, 'trans_lbl_cmp_wit, 'trans_graph) GenericProgram_.t ->
        critical_transitions:('trans_lbl, 'trans_lbl_cmp_wit) _trans_set ->
        ('trans_lbl, 'trans_lbl_cmp_wit, 'trans_graph) GenericProgram_.t MaybeChanged.t;
    }
      -> ('trans_lbl * 'trans_lbl_cmp_wit * 'trans_graph) ProgramTypes.program_modules_meta cfr_

let mk_cfr ~method_name perform_cfr = Cfr { method_name; perform_cfr }

let method_name = function
  | Cfr cfr -> cfr.method_name


let perform_cfr = function
  | Cfr cfr -> cfr.perform_cfr


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

  (* timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
  let compute_timeout_time program appr scc =
    if Base.Set.exists ~f:(fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc then
      0.
    else
      let toplogic_later_trans =
        program |> Program.transitions |> flip Base.Set.diff scc
        |> Base.Set.filter ~f:(fun t -> Bound.is_infinity (Approximation.timebound appr t))
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
  let perform_cfr program ~critical_transitions = PartialEvaluation.apply_cfr critical_transitions program in
  mk_cfr ~method_name:"PartialEvaluationIRankFinder" perform_cfr


let chaining =
  let perform_chaining program ~critical_transitions =
    Preprocessor.lift_to_program (Chaining.transform_graph ~scc:(Option.some critical_transitions)) program
  in
  mk_cfr ~method_name:"Chaining" perform_chaining


let pe_native pe_config =
  let perform_cfr program ~critical_transitions =
    NativePartialEvaluation.ClassicPartialEvaluation.apply_sub_scc_cfr pe_config critical_transitions program
    |> MaybeChanged.changed (* TODO: better solution? *)
  in
  mk_cfr ~method_name:"PartialEvaluationNative" perform_cfr


let pe_native_probabilistic pe_config =
  let perform_cfr program ~critical_transitions =
    NativePartialEvaluation.ProbabilisticPartialEvaluation.apply_sub_scc_cfr pe_config critical_transitions
      program
    |> MaybeChanged.changed (* TODO: better solution? *)
  in
  mk_cfr ~method_name:"PartialEvaluationNativeProbabilistic" perform_cfr
