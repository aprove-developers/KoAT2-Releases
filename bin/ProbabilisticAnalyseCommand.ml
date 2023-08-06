(** Perform a full-fledged probabilistic analysis *)
open Koat2
open OurBase
open Readers
open ProbabilisticProgramModules

module OverapprAnalysis = Analysis.Make(NonProbOverappr)

open Approximation.Probabilistic

let description = "Search for a probabilistic ranking function"

let command = "prob-analyse"

type classic_local = [`MPRF | `TWN | `TWNTransform]

type params = {
  input : string; [@aka ["i"]]
  (** Either an absolute or relative path to the koat input file which defines the probabilistic integer transition system. *)

  preprocessors : Program.t Preprocessor.t list;
    [@enum Preprocessor.(List.map (fun p -> show p, p) all_probabilistic)]
    [@default Preprocessor.all_probabilistic ]
  (** The preprocessors which should be applied before running the actual algorithm. *)

  mprf_depth : int; [@default 1] [@aka ["d"]]
  (** The maximum depth of a Multiphase Ranking Function to bound search space.*)

  classic_local : classic_local list; [@enum [("mprf", `MPRF); ("twn", `TWN); ("twn-transform", `TWNTransform);]] [@default [`MPRF]] [@sep ',']
  (** Choose methods to compute local runtime-bounds: mprf, twn *)

  preprocessing_strategy : Preprocessor.strategy; [@enum Preprocessor.["once", process_only_once; "fixpoint", process_till_fixpoint]] [@default Preprocessor.process_till_fixpoint]
  (** The strategy which should be used to apply the preprocessors. *)

} [@@deriving cmdliner]

let run (params: params) =
  (* Logging.(use_loggers [PRF, Logger.DEBUG; Preprocessor, Logger.DEBUG]); *)
  Logging.(use_loggers [ Preprocessor, Logger.DEBUG; ExpSize, Logger.DEBUG]);
  let (program,_) = (* TODO respect goals *)
    Readers.read_probabilistic_prog_goal_file params.input
  in

  let preprocess =
    Preprocessor.ProbabilisticWithOverappr.process params.preprocessing_strategy params.preprocessors
  in

  Stdio.printf "prog %s\n\n" (Program.to_string_pretty program);

  let classical_analysis_conf =
    let open TWN in
    List.fold_left ~f:(fun conf -> function
        | `MPRF -> { conf with Analysis.run_mprf_depth = Some params.mprf_depth; }
        | `TWN -> { conf with twn_configuration = Some `NoTransformation}
        | `TWNTransform -> { conf with twn_configuration = Some `Transformation}
      )
      ~init:Analysis.default_configuration params.classic_local
  in

  let program =
    ProofOutput.add_to_proof (fun () -> FormattedString.mk_header_big (FormattedString.mk_str "Preprocessing"));
    preprocess program
  in

  let program, class_appr =
    let overappr =
      Type_equal.conv ProbabilisticPrograms.Equalities.program_equalities program
    in
    OverapprAnalysis.improve ~preprocess:identity ~conf:classical_analysis_conf overappr NonProbOverapprApproximation.empty
    |> Tuple2.map
        Type_equal.(conv (sym ProbabilisticPrograms.Equalities.program_equalities))
        coerce_from_nonprob_overappr_approximation
  in

  let prob_appr = ProbabilisticAnalysis.perform_analysis program class_appr in
  print_endline (ClassicalApproximation.to_string program class_appr);
  print_endline (ExpApproximation.to_string program prob_appr);
