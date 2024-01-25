open Koat2
open! OurBase

let command = "cfr"
let description = "Do a control-flow refinement of a full (probabilistic) integer transition system"

type params = {
  probabilistic : bool; [@default false] [@aka [ "p" ]]
  print_input : bool; [@default false]  (** Prints the raw unmodified input before the start *)
  input : string; [@aka [ "i" ]]
      (** Either an absolute or relative path to the koat input file which defines the integer transition system. *)
  output : string option; [@aka [ "o" ]]
      (** An absolute or relative path to the output file, where refined program should end up.
      If ommited, the program will be printed to stdout. *)
  show_steps : bool;  (** Displays the steps of the control flow refinement. *)
  log_level : Logger.level;
      [@enum
        Logger.[ NONE; FATAL; ERROR; WARN; NOTICE; INFO; DEBUG ]
        |> List.map (fun level -> (Logger.name_of_level level, level))]
      [@default Logger.INFO]
      (** The general log level of the loggers. *)
  cfr_method : [ `PartialEvaluationNative | `PartialEvaluationIRankFinder ];
      [@enum [ ("native", `PartialEvaluationNative); ("irankfinder", `PartialEvaluationIRankFinder) ]]
      [@default `PartialEvaluationNative]
      (** Choose the cfr method. One of native, irankfinder, or chaining *)
  abstract : [ `LoopHeads | `FVS ];
      [@enum [ ("loop_heads", `LoopHeads); ("fvs", `FVS) ]] [@default `LoopHeads]
      (** Where to abstract. One of loop_heads or fvs. *)
  refinement_locations : Location.t list option;
      (** List of comma-seperated location names.
          The refinement is executed along all transitions between pairs of these locations
          If this parameter is ommitted *all* transitions are refined *)
}
[@@deriving cmdliner]
(** The shell arguments which can be defined in the console. *)

(* TODO: Program_.to_file should not add the extension.
   1. the behavious is undocument in the Program_.mli
   2. What if the user already provided an extension in it's argument?
   3. What if the user doesn't what an extension, and expects the file to be
   named as he specified?

   Adding the extension should happen wherever the filename is generated
   (probably in the Command Module) and not during serialization.
*)

(** Write a program to a file *)
let sane_program_to_file oc program =
  let open ProgramModules in
  Printf.fprintf oc "(GOAL COMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)\n"
    (Location.to_string (Program.start program))
    (Set.fold
       ~f:(fun str var -> str ^ " " ^ Var.to_string ~to_file:true var)
       (Program.input_vars program) ~init:"")
    (TransitionGraph.fold_edges_e
       (fun t str -> str ^ " " ^ Transition.to_file_string t ^ "\n")
       (Program.graph program) "");
  close_out oc


let prob_program_to_file oc program =
  let open ProbabilisticProgramModules in
  Printf.fprintf oc "(GOAL EXPECTEDCOMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)\n"
    (Location.to_string (Program.start program))
    (Set.fold
       ~f:(fun str var -> str ^ " " ^ Var.to_string ~to_file:true var)
       (Program.input_vars program) ~init:"")
    (Set.fold
       ~f:(fun str gt -> str ^ " " ^ GeneralTransition.to_file_string gt ^ "\n")
       (Program.gts program) ~init:"");
  close_out oc


let run (params : params) =
  let input = params.input |> Fpath.v |> Fpath.normalize |> Fpath.to_string in
  let output =
    match params.output with
    | Some filename -> Stdio.Out_channel.create ~binary:false ~append:false ~fail_if_exists:false filename
    | None -> Stdio.Out_channel.stdout
  in

  Logging.use_loggers [ (Logging.CFR, params.log_level) ];

  let filter_transitions trans =
    Option.map
      ~f:(fun locs_list ->
        let locs = LocationSet.of_list locs_list in
        Set.filter ~f:(fun (l, _, l') -> Set.mem locs l && Set.mem locs l') trans)
      params.refinement_locations
    |? trans
  in

  let transitions_to_refine program = filter_transitions (ProgramModules.Program.transitions program) in
  let probabilistic_transitions_to_refine program =
    filter_transitions (ProbabilisticProgramModules.Program.transitions program)
  in

  match params.cfr_method with
  | `PartialEvaluationNative ->
      let cfr_config : Abstraction.config = { abstract = params.abstract } in
      if params.probabilistic then
        let module PE = NativePartialEvaluation.ProbabilisticPartialEvaluation in
        let program = Readers.read_probabilistic_program input in
        PE.evaluate_transitions cfr_config program (probabilistic_transitions_to_refine program)
        |> prob_program_to_file output
      else
        let module PE = NativePartialEvaluation.ClassicPartialEvaluation in
        let program = Readers.read_file input in
        PE.evaluate_transitions cfr_config program (transitions_to_refine program)
        |> sane_program_to_file output
  | `PartialEvaluationIRankFinder ->
      if params.probabilistic then
        raise (Invalid_argument "only --cfr_method=native supports probabilistic programs")
      else
        let module PartialEvaluation = PartialEvaluation in
        let program = Readers.read_file input in
        PartialEvaluation.apply_cfr (transitions_to_refine program) program
        |> sane_program_to_file output % MaybeChanged.unpack
