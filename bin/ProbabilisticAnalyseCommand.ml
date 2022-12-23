(** Perform a full-fledged probabilistic analysis *)
open Batteries
open Koat2
open Readers
open ProbabilisticProgramModules

module OverapprAnalysis = Analysis.Make(NonProbOverappr)
module ClassicalBounds = Bounds.Make(NonProbOverappr)

open Approximation.Probabilistic

let description = "Search for a probabilistic ranking function"

let command = "prob-analyse"

type params = {
  input : string; [@aka ["i"]]
  (** Either an absolute or relative path to the koat input file which defines the probabilistic integer transition system. *)

  preprocessors : Program.t Preprocessor.t list;
    [@enum Preprocessor.(List.map (fun p -> show p, p) all_probabilistic)]
    [@default Preprocessor.all_probabilistic ]
  (** The preprocessors which should be applied before running the actual algorithm. *)

  mprf_depth : int; [@default 1] [@aka ["d"]]
  (** The maximum depth of a Multiphase Ranking Function to bound search space.*)

  preprocessing_strategy : Program.t Preprocessor.strategy; [@enum Preprocessor.["once", process_only_once; "fixpoint", process_till_fixpoint]] [@default Preprocessor.process_till_fixpoint]
  (** The strategy which should be used to apply the preprocessors. *)

} [@@deriving cmdliner]

let run (params: params) =
  (* Logging.(use_loggers [PRF, Logger.DEBUG; Preprocessor, Logger.DEBUG]); *)
  Logging.(use_loggers [ Preprocessor, Logger.DEBUG; ExpSize, Logger.DEBUG]);
  let (program,_) = (* TODO respect goals *)
    Readers.read_probabilistic_prog_goal_file params.input
  in

  let preprocess =
    Preprocessor.process (module NonProbOverappr) params.preprocessing_strategy params.preprocessors
  in

  Printf.printf "prog %s\n\n" (Program.to_string_pretty program);

  let classical_analysis_conf = { OverapprAnalysis.default_configuration with run_mprf_depth = Some params.mprf_depth } in

  let program, class_appr =
    preprocess program
    |> tap (fun _ -> ProofOutput.add_to_proof (fun () -> FormattedString.mk_header_big (FormattedString.mk_str "Preprocessing")))
    |> fun prog -> ClassicalBounds.find_bounds ~preprocess ~conf:classical_analysis_conf prog (NonProbOverapprApproximation.create prog)
    |> Tuple2.map2 coerce_from_nonprob_overappr_approximation
  in
  let prob_appr = ProbabilisticAnalysis.perform_analysis program class_appr in
  print_endline (ClassicalApproximation.to_string program class_appr);
  print_endline (ExpApproximation.to_string program prob_appr);
