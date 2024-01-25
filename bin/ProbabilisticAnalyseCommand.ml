open Koat2
(** Perform a full-fledged probabilistic analysis *)

open! OurBase
open ProbabilisticProgramModules
module OverapprAnalysis = Analysis.Make (Bounds.Bound) (NonProbOverappr)
open Approximation.Probabilistic

let description = "Perform a probabilistic analysis on a given PIP"
let command = "prob-analyse"

type classic_local = [ `MPRF | `TWN ]

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
      [@enum [ ("mprf", `MPRF); ("twn", `TWN) ]] [@default [ `MPRF ]] [@sep ',']
      (** Choose methods to compute local runtime-bounds: mprf, twn *)
  closed_form_size_bounds : bool; [@default false]  (** If size should be computed by closed forms. *)
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
      [@enum Logging.(List.map (fun l -> (show_logger l, l)) all_available)]
      [@default Logging.all_available]
      [@sep ',']
      [@aka [ "l" ]]
      (** The loggers which should be activated. *)
  log_level : Logger.level;
      [@enum
        Logger.[ NONE; FATAL; ERROR; WARN; NOTICE; INFO; DEBUG ]
        |> List.map (fun level -> (Logger.name_of_level level, level))]
      [@default Logger.NONE]
      (** The general log level of the loggers. *)
  pe : bool;  (** Enable (native) partial evaluation *)
  no_pe_fvs : bool; [@default false]
  pe_k : int; [@default 0]
  pe_update_invariants : bool; [@default true]
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

  let classical_local_conf =
    List.fold_left
      ~f:
        (fun conf -> function
          | `MPRF -> { conf with Analysis.run_mprf_depth = Some params.mprf_depth }
          | `TWN -> { conf with twn = true })
      ~init:Analysis.default_local_configuration params.classic_local
    |> fun conf ->
    if params.closed_form_size_bounds then
      { conf with closed_form_size_bounds = ComputeClosedFormSizeBounds }
    else
      conf
  in

  let conf =
    let cfrs =
      if params.pe then
        [
          CFR.pe_native_probabilistic
            NativePartialEvaluation.
              {
                abstract =
                  (if params.no_pe_fvs then
                     `LoopHeads
                   else
                     `FVS);
                k_encounters = params.pe_k;
                update_invariants = params.pe_update_invariants;
              };
        ]
      else
        []
    in
    ProbabilisticAnalysis.{ classical_local = classical_local_conf; cfrs; compute_refined_plrfs = false }
  in

  let program =
    ProofOutput.add_to_proof FormattedString.(fun () -> mk_str_header_big "Preprocessing");
    preprocess program
  in

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

  let program, apprs = ProbabilisticAnalysis.perform_analysis ~conf program in

  ProofOutput.add_to_proof
    FormattedString.(
      fun () ->
        mk_str_header_big "Results of Probabilistic Analysis"
        <> FormattedString.reduce_header_sizes ~levels_to_reduce:1
             (ExpApproximation.to_formatted ~pretty:true program apprs.appr));
  Printf.printf "Overall expected time bound: %s\n"
    (Bounds.RationalBound.to_string @@ ExpApproximation.program_timebound apprs.appr program);
  if params.show_proof then
    ProofOutput.print_proof params.proof_format
