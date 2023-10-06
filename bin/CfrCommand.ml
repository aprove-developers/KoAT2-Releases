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
      (** An absolute or relative path to the output file, where refined program should end up. *)
  show_steps : bool;  (** Displays the steps of the control flow refinement. *)
  log_level : Logger.level;
      [@enum
        Logger.[ NONE; FATAL; ERROR; WARN; NOTICE; INFO; DEBUG ]
        |> List.map (fun level -> (Logger.name_of_level level, level))]
      [@default Logger.INFO]
      (** The general log level of the loggers. *)
  cfr_method : [ `PartialEvaluationNative | `PartialEvaluationIRankFinder | `Chaining ];
      [@enum
        [
          ("native", `PartialEvaluationNative);
          ("irankfinder", `PartialEvaluationIRankFinder);
          ("chaining", `Chaining);
        ]]
  abstract : [ `LoopHeads | `FVS ]; [@enum [ ("loop_heads", `LoopHeads); ("fvs", `FVS) ]]
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
let sane_program_to_file file program =
  let open ProgramModules in
  let oc = open_out file in
  Printf.fprintf oc "(GOAL COMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)"
    (Location.to_string (Program.start program))
    (Set.fold
       ~f:(fun str var -> str ^ " " ^ Var.to_string ~to_file:true var)
       (Program.input_vars program) ~init:"")
    (TransitionGraph.fold_edges_e
       (fun t str -> str ^ " " ^ Transition.to_file_string t ^ "\n")
       (Program.graph program) "");
  close_out oc


let prob_program_to_file file program =
  let open ProbabilisticProgramModules in
  let oc = open_out file in
  Printf.fprintf oc "(GOAL EXPECTEDCOMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)"
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
    params.output
    |?
    let input_directory, input_filename = Fpath.v input |> Fpath.split_base in
    let input_rem, input_ext = Fpath.split_ext input_filename in
    Fpath.to_string input_rem ^ "-pe"
    |> Fpath.v |> Fpath.add_ext "koat" |> Fpath.append input_directory |> Fpath.to_string
  in

  Logging.use_loggers [ (Logging.CFR, params.log_level) ];

  match params.cfr_method with
  | `PartialEvaluationNative ->
      let cfr_config : NativePartialEvaluation.config =
        { k_encounters = 0; abstract = params.abstract; update_invariants = true }
      in
      if params.probabilistic then
        let module PE = NativePartialEvaluation.ProbabilisticPartialEvaluation in
        Readers.read_probabilistic_program input
        |> PE.evaluate_program cfr_config |> prob_program_to_file output
      else
        let module PE = NativePartialEvaluation.ClassicPartialEvaluation in
        Readers.read_file input |> PE.evaluate_program cfr_config |> sane_program_to_file output
  | `PartialEvaluationIRankFinder ->
      if params.probabilistic then
        raise (Invalid_argument "only --cfr_method=native supports probabilistic programs")
      else
        Readers.read_file input |> PartialEvaluation.applyIrankFinder |> sane_program_to_file output
  | `Chaining -> raise (Invalid_argument "TODO: --cfr_method=chaining not implemented")
