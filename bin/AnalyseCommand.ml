open Koat2
(** Handles shell arguments and computes an upper time-bound. *)

open! OurBase
open ProgramModules
open RVGTypes
open Bounds

let command = "analyse"
let description = "Proceed a full time, cost and size analysis on a given integer transition system"

(** Prints the whole resulting approximation to the shell. *)
let print_all_bounds analysis_type (program : Program.t) (appr : Approximation.t) : unit =
  let form = analysis_type == `Termination in
  Approximation.to_string ~termination_only:form program appr |> print_string


(** Prints the overall timebound of the program to the shell. *)

let print_overall_costbound analysis_type (program : Program.t) (appr : Approximation.t) : unit =
  let print_overall_costbound_complexity (program : Program.t) (appr : Approximation.t) : unit =
    program |> Approximation.program_costbound appr |> Bound.to_string |> print_endline
  in
  let print_overall_termination (program : Program.t) (appr : Approximation.t) : unit =
    program |> Approximation.program_timebound appr |> Bound.to_string ~termination_only:true |> print_endline
  in

  match analysis_type with
  | `Termination -> print_overall_termination program appr
  | `Complexity -> print_overall_costbound_complexity program appr


(** Prints the overall timebound of the program to the shell in the TermComp fashion. *)
let print_termcomp analysis_type (program : Program.t) (appr : Approximation.t) : unit =
  (match analysis_type with
  | `Termination ->
      program |> Approximation.program_timebound appr
      |> Bound.to_string ~termination_only:true
      |> String.uppercase
  | `Complexity ->
      program |> Approximation.program_costbound appr |> Bound.asymptotic_complexity
      |> Bound.show_complexity_termcomp)
  |> print_endline


type local = [ `MPRF | `TWN | `TWNTransform ]
(** TWN for no transformations, TWNTransform for both techniques, TWNTransformJordan to transform only with jordan normal form, TWNTransformGeneral to transform only with the general approach *)

type cfr = [ `PartialEvaluation | `Chaining ]

type params = {
  print_system : bool;  (** Prints the integer transition system at the start as png *)
  print_rvg : bool;  (** Prints the input result variable graph at the start as png *)
  print_input : bool;  (** Prints the raw unmodified input before the start *)
  no_boundsearch : bool; [@aka [ "n" ]]
      (** Disables the search for bounds. Useful if you just want information about the integer transition system via the other options or for debugging purposes. *)
  input : string option; [@aka [ "i" ]]
      (** Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)
  simple_input : bool; [@default false] [@aka [ "s" ]]
      (** If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)
  output_dir : string option; [@aka [ "o" ]]
      (** An absolute or relative path to the output directory, where all generated files should end up. *)
  show_proof : bool;  (** Displays the complexity proof. *)
  proof_format : Formatter.format;
      [@enum
        [ Formatter.Html; Formatter.Markdown; Formatter.Plain ]
        |> List.map (fun f -> (Formatter.format_to_string f, f))]
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
  result : [ `Complexity | `Termination ] -> Program.t -> Approximation.t -> unit;
      [@enum
        [ ("termcomp", print_termcomp); ("all", print_all_bounds); ("overall", print_overall_costbound) ]]
      [@default print_overall_costbound]
      [@aka [ "r" ]]
      (** The kind of output which is deserved. The option "all" prints all time- and sizebounds found in the whole program, the option "overall" prints only the sum of all timebounds. The option "termcomp" prints the approximated complexity class. *)
  preprocessors : Program.t Preprocessor.t list;
      [@enum Preprocessor.(List.map (fun p -> (show p, p)) all_classical)]
      [@default
        Preprocessor.
          [
            InvariantGeneration;
            CutUnsatisfiableTransitions;
            CutUnreachableLocations;
            EliminateNonContributors;
            EliminateTempVars;
          ]]
      (** The preprocessors which should be applied before running the actual algorithm. *)
  preprocessing_strategy : Preprocessor.strategy;
      [@enum Preprocessor.[ ("once", process_only_once); ("fixpoint", process_till_fixpoint) ]]
      [@default Preprocessor.process_till_fixpoint]
      (** The strategy which should be used to apply the preprocessors. *)
  local : local list;
      [@enum [ ("mprf", `MPRF); ("twn", `TWN); ("twn-transform", `TWNTransform) ]]
      [@default [ `MPRF ]]
      [@sep ',']
      (** Choose methods to compute local runtime-bounds: mprf, twn *)
  closed_form_size_bounds : bool; [@default false]  (** If size should be computed by closed forms. *)
  rename : bool; [@default false]  (** If the location names should be normalized to simplified names. *)
  depth : int; [@default 1] [@aka [ "d" ]]
      (** The maximum depth of a Multiphase Ranking Function to bound search space.*)
  cfr : cfr list; [@enum [ ("pe", `PartialEvaluation); ("chain", `Chaining) ]] [@default []] [@sep ',']
      (** Choose methods for local control-flow-refinement: pe (Partial Evaluation) or chain (Chaining) *)
  time_limit_cfr : int; [@default 20]
      (** Limits the time spend maximal on cfr. Default is 180 (seconds). Note that this is not a strict upper bound and more an approximation. We ignore the limit on unbound transitions. Use -1 to set no limit. *)
  timeout : float; [@default 0.]
      (** Makes sure the analysis stops after the specified amount of time. Might result in empty output.*)
  termination : bool; [@default false]  (** Only looks for termination behavior. *)
}
[@@deriving cmdliner]
(** The shell arguments which can be defined in the console. *)

(** Returns a string containing a time-bound and the label of a transition for a specified approximation. *)
let bounded_label_to_string (appr : Approximation.t) (trans : Transition.t) : string =
  String.concat ~sep:""
    [
      "s: ";
      Approximation.timebound appr trans |> Bound.to_string;
      "\n";
      TransitionLabel.to_string (Transition.label trans);
    ]


(** Returns a string containing a size-bound transition and a result variable for a specified approximation. *)
let bounded_rv_to_string (program : Program.t) (appr : Approximation.t) (t, v) =
  let get_lsb (t, v) =
    LocalSizeBound.(sizebound_local program t v |> Option.map ~f:as_bound |? Bound.infinity)
  in
  String.concat ~sep:""
    [
      RV.to_id_string (t, v);
      "\n";
      "Global: ";
      Approximation.sizebound appr t v |> Bound.to_string;
      "\n";
      "Local: ";
      get_lsb (t, v) |> Bound.show ~complexity:false;
    ]


(** Returns a local size-bound for a specified transition and a specified variable. *)
let get_lsb program (t, v) =
  LocalSizeBound.(sizebound_local program t v |> Option.map ~f:as_bound |? Bound.infinity)


let program_to_formatted_string prog = function
  | Formatter.Html -> FormattedString.mk_raw_str (GraphPrint.print_system_pretty_html prog)
  | _ -> FormattedString.Empty


let local_to_string = function
  | `MPRF -> "MPRF"
  | `TWN -> "TWN"


let analysis_type b =
  if b then
    `Termination
  else
    `Complexity


(** Runs KoAT2 on provided parameters. *)
let run (params : params) =
  Timeout.start_time_of_koat2 := Unix.gettimeofday ();
  let logs = List.map ~f:(fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;
  let input = Option.value_or_thunk ~default:read_line params.input in
  let preprocess = Preprocessor.StandardProgram.process params.preprocessing_strategy params.preprocessors in

  (* TODO improve parser arguments to avoid the case of multiple twns *)
  let bounds_conf =
    let open Analysis in
    let check_twn_already_set c =
      if Option.is_some c then
        raise
          (Invalid_argument
             "--local commands are not correct. Use 'mprf' or at most one of 'twn', 'twn-transform'")
    in
    {
      run_mprf_depth =
        (if List.mem ~equal:Poly.( = ) params.local `MPRF then
           Some params.depth
         else
           None);
      twn_configuration =
        List.fold_left
          ~f:
            (fun twn_conf -> function
              | `TWN ->
                  check_twn_already_set twn_conf;
                  Some `NoTransformation
              | `TWNTransform ->
                  check_twn_already_set twn_conf;
                  Some `Transformation
              | _ -> twn_conf)
          ~init:None params.local;
      closed_form_size_bounds =
        (if params.closed_form_size_bounds then
           ComputeClosedFormSizeBounds
         else
           NoClosedFormSizeBounds);
      analysis_type = analysis_type params.termination;
      cfr_configuration =
        (match params.cfr with
        | [] -> NoCFR
        | l -> PerformCFR l);
    }
  in
  let input_filename =
    if params.simple_input then
      "dummyname"
    else
      input |> Fpath.v |> Fpath.normalize |> Fpath.rem_ext |> Fpath.filename
  and output_dir =
    Option.map ~f:Fpath.v params.output_dir
    |?
    if params.simple_input then
      Fpath.v "."
    else
      input |> Fpath.v |> Fpath.parent
  in
  (if params.print_input then
     let program_str =
       if params.simple_input then
         input
       else
         Stdio.In_channel.read_lines input |> String.concat ~sep:"\n"
     in
     print_string (program_str ^ "\n\n"));
  let program =
    input
    |> Readers.read_input ~termination:params.termination
         ~rename:(List.mem ~equal:Poly.( = ) params.cfr `PartialEvaluation || params.rename)
         params.simple_input
    |> tap (fun prog ->
           ProofOutput.add_to_proof @@ fun () ->
           FormattedString.(
             mk_header_big (mk_str "Initial Problem")
             <> mk_paragraph (Program.to_formatted_string ~pretty:true prog)
             <> program_to_formatted_string prog params.proof_format))
  in
  Timeout.timed_run params.timeout
    ~action:(fun () ->
      print_string
        "TIMEOUT: Complexity analysis of the given ITS stopped as the given timelimit has been exceeded!\n")
    (fun () ->
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
               <> program_to_formatted_string prog params.proof_format))
      |> tap (fun (program, appr) ->
             if params.print_system then
               GraphPrint.print_system ~format:"png"
                 ~label:(TransitionLabel.to_string % Transition.label)
                 ~outdir:output_dir ~file:input_filename program)
      |> tap (fun (program, appr) ->
             if params.print_rvg then
               GraphPrint.print_rvg ~format:"png" ~label:RV.to_id_string ~outdir:output_dir
                 ~file:input_filename program)
      |> (fun (program, appr) ->
           if params.no_boundsearch then
             (program, appr)
           else if
             Set.exists ~f:(TransitionLabel.negative_costs % Tuple3.second) (Program.transitions program)
           then
             (program, appr)
           else
             let module Analysis = Analysis.Make (ProgramModules) in
             Analysis.improve ~conf:bounds_conf ~preprocess ~time_cfr:params.time_limit_cfr program appr)
      |> tap (fun (program, appr) -> params.result (analysis_type params.termination) program appr)
      |> tap (fun (program, appr) ->
             ProofOutput.add_to_proof (fun () ->
                 Approximation.to_formatted ~pretty:true ~show_initial:false
                   ~termination_only:params.termination program appr))
      |> tap (fun (program, appr) ->
             if params.print_system then
               GraphPrint.print_system ~format:"png" ~label:(bounded_label_to_string appr) ~outdir:output_dir
                 ~file:input_filename program)
      |> tap (fun (program, appr) ->
             if params.print_rvg then
               GraphPrint.print_rvg ~format:"png" ~label:(bounded_rv_to_string program appr)
                 ~outdir:output_dir ~file:input_filename program))
  |> ignore;
  if params.show_proof then (
    print_string "\n\n";
    ProofOutput.print_proof params.proof_format);
  if params.log_level == NONE && not (List.mem ~equal:Poly.( = ) params.cfr `PartialEvaluation) then
    ignore (Sys.command ("rm -f -r ./tmp_" ^ !PartialEvaluation.uid))
