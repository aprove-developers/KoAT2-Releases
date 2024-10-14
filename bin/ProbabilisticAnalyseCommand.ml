open Koat2
(** Perform a full-fledged probabilistic analysis *)

open! OurBase
open ProbabilisticProgramModules
module OverapprAnalysis = Analysis.Make (Bounds.Bound) (NonProbOverappr)

type classic_local = [ `MPRF | `TWN | `TWNLOG | `UNSOLVABLE ]

let description = "Perform a probabilistic analysis on a given PIP"
let command = "prob-analyse"

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
      [@enum [ ("mprf", `MPRF); ("twn", `TWN); ("twnlog", `TWNLOG); ("unsolvable", `UNSOLVABLE) ]]
      [@default [ `MPRF ]]
      [@sep ',']
      (** Choose methods to compute local runtime-bounds: mprf, twn, twnlog or unsolvable *)
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
  result : [ `PrintTermcomp | `PrintAllBounds | `PrintOverallCostbound ];
      [@enum [ ("termcomp", `PrintTermcomp); ("all", `PrintAllBounds); ("overall", `PrintOverallCostbound) ]]
      [@default `PrintOverallCostbound]
      [@aka [ "r" ]]
      (** The kind of output which is deserved. The option "all" prints all time- and sizebounds found in the whole program, the option "overall" prints only the sum of all timebounds. The option "termcomp" prints the approximated complexity class. *)
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
  pe : bool;  (** Enable partial evaluation *)
  pe_fvs : bool; [@default false]
  pe_k : int; [@default 0]
  pe_update_invariants : bool; [@default true]
  timeout : float; [@default 0.]
      (** Makes sure the analysis stops after the specified amount of time. Might result in empty output.*)
}
[@@deriving cmdliner]

exception UnsupportedGoal of string

(* TODO respect other goals *)
let bound_pair_for_goal (goal : Goal.probabilistic Goal.goal) : (module BoundPair.T) =
  match goal with
  | Goal.AlmostSureTermination -> (module BoundPair.AST)
  | Goal.ExpectedComplexity -> (module BoundPair.PAST)
  | _ -> raise (UnsupportedGoal (Goal.to_string goal))


module AnalysisConfig (BP : BoundPair.T) = struct
  module ProbabilisticAnalysis = ProbabilisticAnalysis.Make (BP)

  let get_configuration (params : params) : ProbabilisticAnalysis.configuration =
    {
      cfrs =
        (if params.pe then
           [
             CFR.pe_probabilistic
               {
                 abstract =
                   (if params.pe_fvs then
                      `FVS
                    else
                      `LoopHeads);
               };
           ]
         else
           []);
      classical_local =
        {
          run_mprf_depth =
            (if List.mem ~equal:Poly.( = ) params.classic_local `MPRF then
               Some params.mprf_depth
             else
               None);
          twn =
            List.exists ~f:(( == ) `TWN) params.classic_local
            || List.exists ~f:(( == ) `TWNLOG) params.classic_local;
          twnlog = List.exists ~f:(( == ) `TWNLOG) params.classic_local;
          commuting = false;
          unsolvable = List.exists ~f:(( == ) `UNSOLVABLE) params.classic_local;
          closed_form_size_bounds = Analysis.NoClosedFormSizeBounds;
          goal = ProbabilisticAnalysis.default_configuration.classical_local.goal;
        };
    }
end

module AnalysisRunner (BP : BoundPair.T) = struct
  module ProbabilisticAnalysis = ProbabilisticAnalysis.Make (BP)
  open Approximation.Probabilistic (BP)

  let run_analysis (conf : ProbabilisticAnalysis.configuration) (program : Program.t)
      (preprocess : Program.t -> Program.t) : Program.t * ExpApproximation.t =
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

    (program, apprs.appr)
end

module ResultPrinter (BP : BoundPair.T) = struct
  open Approximation.Probabilistic (BP)

  (** Prints the whole resulting approximation to the shell. *)
  let print_all_bounds (program : Program.t) (appr : ExpApproximation.t) : unit =
    ExpApproximation.to_string program appr |> print_string


  (** Prints the overall timebound of the program to the shell. *)
  let print_overall_costbound (program : Program.t) (appr : ExpApproximation.t) : unit =
    ExpApproximation.program_costbound appr program |> BP.ProbBound.to_string |> print_endline


  (** Prints the overall timebound of the program to the shell in the TermComp fashion. *)
  let print_termcomp (program : Program.t) (appr : ExpApproximation.t) : unit =
    ExpApproximation.program_costbound appr program
    |> BP.ProbBound.asymptotic_complexity |> BP.ProbBound.show_complexity_termcomp |> print_endline


  let print_result = function
    | `PrintOverallCostbound -> print_overall_costbound
    | `PrintTermcomp -> print_termcomp
    | `PrintAllBounds -> print_all_bounds
end

let run (params : params) =
  Timeout.start_time_of_koat2 := Unix.gettimeofday ();
  let logs = List.map ~f:(fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;

  let program, goal = Readers.read_probabilistic_prog_goal_file params.input in

  let (module BP) = bound_pair_for_goal goal in

  let conf =
    let module AnalysisConfig = AnalysisConfig (BP) in
    AnalysisConfig.get_configuration params
  in

  let preprocess =
    Preprocessor.ProbabilisticWithOverappr.process params.preprocessing_strategy params.preprocessors
  in

  Timeout.timed_run params.timeout
    ~action:(fun () ->
      print_string
        "TIMEOUT: Complexity analysis of the given ITS stopped as the given timelimit has been exceeded!\n")
    (fun () ->
      let program, appr =
        let module AnalysisRunner = AnalysisRunner (BP) in
        AnalysisRunner.run_analysis conf program preprocess
      in

      let module ResultPrinter = ResultPrinter (BP) in
      ResultPrinter.print_result params.result program appr)
  |> ignore;

  if params.show_proof then
    ProofOutput.print_proof params.proof_format
