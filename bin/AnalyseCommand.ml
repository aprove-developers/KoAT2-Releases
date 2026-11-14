open Koat2
(** Handles shell arguments and computes an upper time-bound. *)

open! OurBase
open SublinerHelper
open ProgramModules
open RVGTypes
open Bounds

exception FormatExn of string

let command = "analyse"
let description = "Proceed a full time, cost and size analysis on a given integer transition system"

type mode = [ `Complexity | `Termination | `FairTermination ]

let termination = function
  | `Termination
  | `FairTermination ->
      true
  | `Complexity -> false


type 'bound approximation_t =
  ( (Transition.t, 'bound, Transition.comparator_witness) TransitionApproximation.transition_approximation_t,
    (RV.t, 'bound, RV.comparator_witness) SizeApproximation.size_approximation_t )
  Approximation.approximation_t

(** Prints the whole resulting approximation to the shell. *)
let print_all_bounds mode (type b) (module B : BoundType.Bound with type t = b) (program : Program.t)
    (appr : b approximation_t) : unit =
  (* Here, (type b) is a "locally abstract type". *)
  let module Approximation = Approximation.MakeForClassicalAnalysis (B) (ProgramModules) in
  Approximation.to_string ~termination_only:(termination mode) program appr |> print_string


(** Prints the overall timebound of the program to the shell. *)
let print_overall_costbound mode (type b) (module B : BoundType.Bound with type t = b) (program : Program.t)
    (appr : b approximation_t) : unit =
  let module Approximation = Approximation.MakeForClassicalAnalysis (B) (ProgramModules) in
  match mode with
  | `FairTermination -> Approximation.program_costbound_fair appr program |> B.to_string |> print_endline
  | _ -> Approximation.program_costbound appr program |> B.to_string |> print_endline


(** Prints the overall timebound of the program to the shell in the TermComp fashion. *)
let print_termcomp (type b) (module B : BoundType.Bound with type t = b) (program : Program.t)
    (appr : b approximation_t) : unit =
  let module Approximation = Approximation.MakeForClassicalAnalysis (B) (ProgramModules) in
  Approximation.program_costbound appr program
  |> B.asymptotic_complexity |> B.show_complexity_termcomp |> print_endline


(** Prints the overall timebound of the program to the shell in the TermComp fashion with logaritms. *)
let print_termcompLog (type b) (module B : BoundType.Bound with type t = b) (program : Program.t)
    (appr : b approximation_t) : unit =
  let module Approximation = Approximation.MakeForClassicalAnalysis (B) (ProgramModules) in
  Approximation.program_costbound appr program
  |> B.asymptotic_complexity |> B.show_complexity_termcomp_log |> print_endline


let print_result mode = function
  | `PrintOverallCostbound -> print_overall_costbound mode
  | `PrintTermcomp -> print_termcomp
  | `PrintTermcompLog -> print_termcompLog
  | `PrintAllBounds -> print_all_bounds mode


type local = [ `MPRF | `TWN | `TWNLOG | `UNSOLVABLE ]
type size = [ `CLOSED | `COMMUTING ]
type cfr = [ `PartialEvaluation | `Chaining ]

type params = {
  print_system : bool;  (** Prints the integer transition system at the start as png *)
  print_rvg : bool;  (** Prints the input result variable graph at the start as png *)
  print_input : bool;  (** Prints the raw unmodified input before the start *)
  no_boundsearch : bool; [@names [ "no-boundsearch"; "n" ]]
      (** Disables the search for bounds. Useful if you just want information about the integer transition system via the other options or for debugging purposes. *)
  input : string option; [@names [ "input"; "i" ]]
      (** Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)
  output_dir : string option; [@names [ "output-dir"; "o" ]]
      (** An absolute or relative path to the output directory, where all generated files should end up. *)
  show_proof : bool;  (** Displays the complexity proof. *)
  proof_format :
    (Formatter.format
    [@conv
      mk_enum
        ([ Formatter.Html; Formatter.Markdown; Formatter.Plain ]
        |> List.map ~f:(fun f -> (Formatter.format_to_string f, f)))]);
      [@default Formatter.Plain]
      (** What should be the output format of the proof. html, markdown, or plain? *)
  logs :
    (Logging.logger[@conv mk_enum Logging.(List.map ~f:(fun l -> (show_logger l, l)) all_available)]) list;
      [@default Logging.all_available] [@sep ','] [@names [ "logs"; "l" ]]
      (** The loggers which should be activated. *)
  log_level :
    (Logger.level
    [@conv
      mk_enum
        (Logger.[ NONE; FATAL; ERROR; WARN; NOTICE; INFO; DEBUG ]
        |> List.map ~f:(fun level -> (Logger.name_of_level level, level)))]);
      [@default Logger.NONE]
      (** The general log level of the loggers. *)
  result :
    ([ `PrintTermcomp | `PrintAllBounds | `PrintOverallCostbound | `PrintTermcompLog ]
    [@conv
      mk_enum
        [
          ("termcomp", `PrintTermcomp);
          ("termcompLog", `PrintTermcompLog);
          ("all", `PrintAllBounds);
          ("overall", `PrintOverallCostbound);
        ]]);
      [@default `PrintOverallCostbound] [@names [ "result"; "r" ]]
      (** The kind of output which is desired. The option "all" prints all time- and sizebounds found in the whole program, the option "overall" prints only the sum of all timebounds. The option "termcomp" prints the approximated complexity class by overapproximating logarithms. The option "termcompLog" prints the approximated complexity class and also handles logarithms. *)
  format : ([ `ARI | `KOAT ][@conv mk_enum [ ("ari", `ARI); ("koat", `KOAT) ]]) Option.t;
      (** Specify the input format. If not provided, it it inferred from the file extension *)
  preprocessors :
    (Program.t Preprocessor.t[@conv mk_enum Preprocessor.(List.map ~f:(fun p -> (show p, p)) all_classical)])
    list;
      [@default Preprocessor.default_classical]
      (** The preprocessors which should be applied before running the actual algorithm. *)
  preprocessing_strategy :
    (Preprocessor.strategy
    [@conv mk_enum Preprocessor.[ ("once", process_only_once); ("fixpoint", process_till_fixpoint) ]]);
      [@default Preprocessor.process_till_fixpoint]
      (** The strategy which should be used to apply the preprocessors. *)
  local :
    (local
    [@conv mk_enum [ ("mprf", `MPRF); ("twn", `TWN); ("twnlog", `TWNLOG); ("unsolvable", `UNSOLVABLE) ]])
    list;
      [@default [ `MPRF ]] [@sep ',']
      (** Choose methods to compute local runtime-bounds: mprf, twn, twnlog, or unsolvable *)
  size : (size[@conv mk_enum [ ("closed", `CLOSED); ("commuting", `COMMUTING) ]]) list;
      [@default []] [@sep ',']
      (** Choose methods to compute size bounds (additionally to TOPLAS'16): closed-form size bounds *)
  rename : bool;  (** If the location names should be normalized to simplified names. *)
  mprf_depth : int; [@default 1] [@names [ "mprf-depth"; "d" ]]
      (** The maximum depth of a Multiphase Ranking Function to bound search space.*)
  cfr : (cfr[@conv mk_enum [ ("pe", `PartialEvaluation); ("chain", `Chaining) ]]) list;
      [@default []] [@sep ',']
      (** Choose methods for local control-flow-refinement: pe (Partial Evaluation) or chain (Chaining) *)
  pe_fvs : bool;
  time_limit_cfr : int; [@default 20]
      (** Limits the time spend maximal on cfr. Default is 180 (seconds). Note that this is not a strict upper bound and more an approximation. We ignore the limit on unbound transitions. Use -1 to set no limit. *)
  timeout : float; [@default 0.]
      (** Makes sure the analysis stops after the specified amount of time. Might result in empty output.*)
  mode :
    (mode
    [@conv
      mk_enum
        [ ("complexity", `Complexity); ("termination", `Termination); ("fairTermination", `FairTermination) ]]);
      [@default `Complexity] [@names [ "mode"; "m" ]]
      (** The analysis goal: "complexity" infers runtime/cost bounds, "termination" only proves termination, and "fairTermination" proves that every run only executes finitely many transitions with non-zero cost. *)
}
[@@deriving subliner]
(** The shell arguments which can be defined in the console. *)

(** Returns a string containing a time-bound and the label of a transition for a specified approximation. *)
let bounded_label_to_string (type b) (module B : BoundType.Bound with type t = b) (appr : b approximation_t)
    (trans : Transition.t) : string =
  let module Approximation = Approximation.MakeForClassicalAnalysis (B) (ProgramModules) in
  String.concat ~sep:""
    [
      "s: ";
      Approximation.timebound appr trans |> B.to_string;
      "\n";
      TransitionLabel.to_string (Transition.label trans);
    ]


let print_lsb lsb =
  if Option.is_none lsb then
    Bound.show ~complexity:false Bound.infinity
  else
    let lsb = Option.value_exn lsb in
    if not @@ PolyFunctionCall.has_function_calls lsb then
      Bound.show ~complexity:false (Bound.of_poly (PolyFunctionCall.to_poly lsb))
    else
      PolyFunctionCall.to_string_pretty lsb


(** Returns a string containing a size-bound transition and a result variable for a specified approximation. *)
let bounded_rv_to_string (type b) (module B : BoundType.Bound with type t = b) (program : Program.t)
    (appr : b approximation_t) (t, v) =
  let get_lsb (t, v) = LocalSizeBound.(sizebound_local program t v |> Option.map ~f:as_poly) in
  let module Approximation = Approximation.MakeForClassicalAnalysis (B) (ProgramModules) in
  String.concat ~sep:""
    [
      RV.to_id_string (t, v);
      "\n";
      "Global: ";
      Approximation.sizebound appr t v |> B.to_string;
      "\n";
      "Local: ";
      print_lsb @@ get_lsb (t, v);
    ]


let program_to_formatted_string prog = function
  | Formatter.Html -> FormattedString.mk_raw_str (GraphPrint.print_system_pretty_html prog)
  | _ -> FormattedString.Empty


let dependency_graph_to_formatted_string dg = function
  | Formatter.Html ->
      FormattedString.mk_raw_str (GraphPrint.MakeForDependencyGraph.print_system_pretty_html dg)
  | _ -> FormattedString.Empty


let local_to_string = function
  | `MPRF -> "MPRF"
  | `TWN -> "TWN"
  | `UNSOLVABLE -> "unsolvable loops"


module MakeAnalysis (Bound : BoundType.Bound) = struct
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (ProgramModules)
  module Analysis = Analysis.Classical (Bound)
  open ProgramModules

  let run_analysis (analysis_conf : Analysis.allowed_conf_type) program params input_filename output_dir =
    let preprocess =
      Preprocessor.StandardProgram.process params.preprocessing_strategy params.preprocessors
    in
    (program, Approximation.empty)
    |> tap (fun _ ->
        ProofOutput.add_to_proof (fun () ->
            FormattedString.mk_header_big (FormattedString.mk_str "Preprocessing")))
    |> Tuple2.map1 preprocess
    |> tap (fun (prog, _) ->
        ProofOutput.add_to_proof @@ fun () ->
        FormattedString.(
          mk_header_big (mk_str "Problem after Preprocessing")
          <> mk_paragraph (Program.to_formatted_string ~pretty:true prog)
          <> program_to_formatted_string prog params.proof_format
          <> dependency_graph_to_formatted_string (Program.dependency_graph prog) params.proof_format))
    |> tap (fun (program, appr) ->
        if params.print_system then
          GraphPrint.print_system ~format:"png"
            ~label:(TransitionLabel.to_string % Transition.label)
            ~outdir:output_dir ~file:input_filename program)
    |> tap (fun (program, appr) ->
        if params.print_rvg then
          GraphPrint.print_rvg ~format:"png" ~label:RV.to_id_string ~outdir:output_dir ~file:input_filename
            program)
    |> (fun (program, appr) ->
    if params.no_boundsearch then
      (program, appr)
    else
      Analysis.improve ~fairTermination:(params.mode == `FairTermination) ~conf:analysis_conf ~preprocess
        ~time_cfr:params.time_limit_cfr program appr)
    |> tap (fun (program, appr) -> print_result params.mode params.result (module Bound) program appr)
    |> tap (fun (program, appr) ->
        ProofOutput.add_to_proof (fun () ->
            Approximation.to_formatted ~pretty:true ~show_initial:false
              ~termination_only:(termination params.mode) program appr))
    |> tap (fun (program, appr) ->
        if params.print_system then
          GraphPrint.print_system ~format:"png"
            ~label:(bounded_label_to_string (module Bound) appr)
            ~outdir:output_dir ~file:input_filename program)
    |> ignore
end

(** Runs KoAT2 on provided parameters. *)
let run (params : params) =
  Timeout.start_time_of_koat2 := Unix.gettimeofday ();
  let logs = List.map ~f:(fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;
  let input = Option.value_or_thunk ~default:read_line params.input in

  let run_analysis =
    let build_complete_conf goal closed_form_size_bounds =
      let open Analysis in
      {
        local_configuration =
          {
            run_mprf_depth =
              (if List.mem ~equal:Poly.( = ) params.local `MPRF then
                 Some params.mprf_depth
               else
                 None);
            twn = List.exists ~f:(( == ) `TWN) params.local || List.exists ~f:(( == ) `TWNLOG) params.local;
            twnlog = List.exists ~f:(( == ) `TWNLOG) params.local;
            commuting = List.exists ~f:(( == ) `COMMUTING) params.size;
            unsolvable = List.exists ~f:(( == ) `UNSOLVABLE) params.local;
            closed_form_size_bounds;
            goal;
          };
        cfrs =
          List.map
            ~f:(function
              | `Chaining -> CFR.chaining
              | `PartialEvaluation ->
                  let pe_config =
                    Abstraction.
                      {
                        abstract =
                          (if params.pe_fvs then
                             `FVS
                           else
                             `LoopHeads);
                      }
                  in
                  CFR.pe pe_config)
            params.cfr;
      }
    in
    match params.mode with
    | `Termination
    | `FairTermination ->
        let module AnalysisTermination = MakeAnalysis (Bounds.BinaryBound) in
        let goal = Analysis.Termination in
        let closed_form_size_bounds = Analysis.NoClosedFormSizeBounds in
        let conf = build_complete_conf goal closed_form_size_bounds in
        AnalysisTermination.run_analysis conf
    | `Complexity ->
        let module AnalysisComplexity = MakeAnalysis (Bounds.Bound) in
        let goal = Analysis.Complexity in
        let closed_form_size_bounds =
          if List.exists ~f:(fun x -> x == `CLOSED || x == `COMMUTING) params.size then
            Analysis.ComputeClosedFormSizeBounds
          else
            Analysis.NoClosedFormSizeBounds
        in
        let conf = build_complete_conf goal closed_form_size_bounds in
        AnalysisComplexity.run_analysis conf
  in
  let input_filename, input_filename_ext =
    let name, ext = Fpath.v input |> Fpath.normalize |> Fpath.split_ext in
    (Fpath.filename name, ext)
  in
  let output_dir = Option.map ~f:Fpath.v params.output_dir |? (input |> Fpath.v |> Fpath.parent) in
  (if params.print_input then
     let program_str = Stdio.In_channel.read_lines input |> String.concat ~sep:"\n" in
     print_string (program_str ^ "\n\n"));
  let program =
    match (params.format, input_filename_ext) with
    | Some `KOAT, _
    | None, ".koat" ->
        KoatReaders.read_input ~termination:(termination params.mode) ~rename:params.rename false input
    | Some `ARI, _
    | None, ".ari" ->
        AriParser.from_file_exn input
        |> tap (fun prog ->
            ProofOutput.add_to_proof @@ fun () ->
            FormattedString.(
              mk_header_big (mk_str "Initial Problem")
              <> mk_paragraph (Program.to_formatted_string ~pretty:true prog)
              <> program_to_formatted_string prog params.proof_format
              <> dependency_graph_to_formatted_string (Program.dependency_graph prog) params.proof_format))
    | None, ext -> raise (FormatExn ext)
  in
  Timeout.timed_run params.timeout
    ~action:(fun () ->
      print_string
        "TIMEOUT: Complexity analysis of the given ITS stopped as the given timelimit has been exceeded!\n")
    (fun () -> run_analysis program params input_filename output_dir)
  |> ignore;
  if params.show_proof then (
    print_string "\n\n";
    ProofOutput.print_proof params.proof_format)
