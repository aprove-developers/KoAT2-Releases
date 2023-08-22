open Koat2
(** Perform a full-fledged probabilistic analysis *)

open! OurBase
open ProbabilisticProgramModules
module OverapprAnalysis = Analysis.Make (NonProbOverappr)
open Approximation.Probabilistic

let description = "Search for a probabilistic ranking function"
let command = "prob-analyse"

type classic_local = [ `MPRF | `TWN | `TWNTransform ]

type params = {
  input : string; [@aka [ "i" ]]
      (** Either an absolute or relative path to the koat input file which defines the probabilistic integer transition system. *)
  preprocessors : Program.t Preprocessor.t list;
      [@enum Preprocessor.(List.map (fun p -> (show p, p)) all_probabilistic)]
      [@default Preprocessor.all_probabilistic]
      (** The preprocessors which should be applied before running the actual algorithm. *)
  mprf_depth : int; [@default 1] [@aka [ "d" ]]
      (** The maximum depth of a Multiphase Ranking Function to bound search space.*)
  classic_local : classic_local list;
      [@enum [ ("mprf", `MPRF); ("twn", `TWN); ("twn-transform", `TWNTransform) ]]
      [@default [ `MPRF ]]
      [@sep ',']
      (** Choose methods to compute local runtime-bounds: mprf, twn *)
  preprocessing_strategy : Preprocessor.strategy;
      [@enum Preprocessor.[ ("once", process_only_once); ("fixpoint", process_till_fixpoint) ]]
      [@default Preprocessor.process_till_fixpoint]
      (** The strategy which should be used to apply the preprocessors. *)
  show_proof : bool;  (** Displays the complexity proof. *)
  proof_format : Formatter.format;
      [@enum Formatter.all_formats |> List.map (fun f -> (Formatter.format_to_string f, f))]
      [@default Formatter.Plain]
      (** What should be the output format of the proof. html, markdown, or plain? *)
  logs : Logging.logger list;
      [@enum Logging.(List.map (fun l -> (show_logger l, l)) loggers)]
      [@default Logging.all]
      [@sep ',']
      [@aka [ "l" ]]
      (** The loggers which should be activated. *)
  log_level : Logger.level;
      [@enum
        Logger.[ NONE; FATAL; ERROR; WARN; NOTICE; INFO; DEBUG ]
        |> List.map (fun level -> (Logger.name_of_level level, level))]
      [@default Logger.NONE]
      (** The general log level of the loggers. *)
}
[@@deriving cmdliner]

let run (params : params) =
  let logs = List.map ~f:(fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;
  let program, _ =
    (* TODO respect goals *)
    Readers.read_probabilistic_prog_goal_file params.input
  in

  let preprocess =
    Preprocessor.ProbabilisticWithOverappr.process params.preprocessing_strategy params.preprocessors
  in

  let classical_analysis_conf =
    List.fold_left
      ~f:
        (fun conf -> function
          | `MPRF -> { conf with Analysis.run_mprf_depth = Some params.mprf_depth }
          | `TWN -> { conf with twn_configuration = Some `NoTransformation }
          | `TWNTransform -> { conf with twn_configuration = Some `Transformation })
      ~init:Analysis.default_configuration params.classic_local
  in

  let program =
    ProofOutput.add_to_proof FormattedString.(fun () -> mk_str_header_big "Preprocessing");
    preprocess program
  in

  let program, class_appr =
    ProofOutput.add_to_proof FormattedString.(fun () -> mk_str_header_big "Classical Analysis");
    let overappr =
      ProofOutput.add_to_proof
        FormattedString.(fun () -> mk_str_header_small "Classical Program after Preprocessing");
      Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program
      |> tap (fun program ->
             ProofOutput.add_to_proof_with_format
               FormattedString.(
                 fun format ->
                   let module GP = GraphPrint.MakeFromClassical (NonProbOverappr) in
                   NonProbOverappr.Program.to_formatted_string ~pretty:true program
                   <>
                   match format with
                   | Formatter.Html -> mk_raw_str (GP.print_system_pretty_html program)
                   | _ -> Empty))
    in
    OverapprAnalysis.improve ~preprocess:identity ~conf:classical_analysis_conf overappr
      NonProbOverapprApproximation.empty
    |> Tuple2.map
         Type_equal.(conv (sym ProbabilisticPrograms.Equalities.program_equalities))
         coerce_from_nonprob_overappr_approximation
  in

  ProofOutput.add_to_proof
    FormattedString.(
      fun () ->
        mk_str_header_big "Results of Classical Analysis"
        <> FormattedString.reduce_header_sizes
             (ClassicalApproximation.to_formatted ~pretty:true program class_appr));

  ProofOutput.add_to_proof FormattedString.(fun () -> mk_str_header_big "Probabilistic Analysis");
  ProofOutput.add_to_proof_with_format
    FormattedString.(
      fun format ->
        let module GP = GraphPrint.ProbabilisticGraphPrint in
        mk_str_header_small "Probabilistic Program after Preprocessing"
        <> Program.to_formatted_string ~pretty:true program
        <>
        match format with
        | Formatter.Html -> mk_raw_str (GP.print_system_pretty_html program)
        | _ -> Empty);

  let prob_appr =
    ProbabilisticAnalysis.perform_analysis ~classic_conf:classical_analysis_conf program class_appr
  in

  ProofOutput.add_to_proof
    FormattedString.(
      fun () ->
        mk_str_header_big "Results of Probabilistic Analysis"
        <> FormattedString.reduce_header_sizes ~levels_to_reduce:1
             (ExpApproximation.to_formatted ~pretty:true program prob_appr));
  Printf.printf "Overall expected time bound: %s\n"
    (Bounds.RationalBound.to_string @@ ExpApproximation.program_timebound prob_appr program);
  if params.show_proof then
    ProofOutput.print_proof params.proof_format
