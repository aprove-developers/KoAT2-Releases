open Batteries
open ProgramTypes
open RVGTypes
   
(** Prints the whole resulting approximation to the shell. *)
let print_all_bounds (program: Program.t) (appr: Approximation.t): unit =
  print_string (Approximation.to_string program appr)

(** Prints the overall timebound of the program to the shell. *)
let print_overall_timebound (program: Program.t) (appr: Approximation.t): unit =
  print_string ((Bound.to_string Approximation.(TransitionApproximation.sum (time appr) program))^"\n")


(** The shell arguments which can be defined in the console. *)
type main_params = {

    print_system : bool;
    (** Prints the integer transition system at the start as png *)

    print_rvg : bool;
    (** Prints the input result variable graph at the start as png *)

    print_input : bool;
    (** Prints the raw unmodified input before the start *)

    no_boundsearch : bool; [@aka ["n"]]
    (** Disables the search for bounds. Useful if you just want information about the integer transition system via the other options or for debugging purposes. *)

    input : string option; [@aka ["i"]]
    (** Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)

    simple_input : bool; [@default false] [@aka ["s"]]
    (** If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)
    
    output_dir : string option; [@aka ["o"]]
    (** An absolute or relative path to the output directory, where all generated files should end up. *)

    logs : Logging.logger list; [@enum Logging.(List.map (fun l -> show_logger l, l) loggers)] [@default Logging.all] [@sep ','] [@aka ["l"]]
    (** The loggers which should be activated. *)

    log_level : Logger.level; [@enum Logger.([NONE; FATAL; ERROR; WARN; NOTICE; INFO; DEBUG]) |> List.map (fun level -> Logger.name_of_level level, level)] [@default Logger.NONE]
    (** The general log level of the loggers. *)
    
    result : (Program.t -> Approximation.t -> unit); [@enum ["all", print_all_bounds; "overall", print_overall_timebound]] [@default print_overall_timebound] [@aka ["r"]]
    (** The kind of output which is deserved. The option "all" prints all time- and sizebounds found in the whole program, the option "overall" prints only the sum of all timebounds. *)
    
    preprocessors : Preprocessor.t list; [@enum Preprocessor.(List.map (fun p -> show p, p) all)] [@default Preprocessor.all]
    (** The preprocessors which should be applied before running the actual algorithm. *)
    
    preprocessing_strategy : Preprocessor.strategy; [@enum Preprocessor.["once", process_only_once; "fixpoint", process_til_fixpoint]] [@default Preprocessor.process_til_fixpoint]
    (** The strategy which should be used to apply the preprocessors. *)
    
  } [@@deriving cmdliner]

let bounded_label_to_string (appr: Approximation.t) (label: TransitionLabel.t): string =
  let get accessor = Location.of_string (accessor label) in
  String.concat "" ["Timebound: ";
                    Approximation.timebound appr (get TransitionLabel.start, label, get TransitionLabel.target) |> Bound.to_string;
                    "\n";
                    TransitionLabel.to_string label]

let bounded_rv_to_string (program_vars: VarSet.t) kind (appr: Approximation.t) (t,v) =
  let comp = function
    | `Lower -> "<="
    | `Upper -> ">="
  in
  String.concat "" ["Global: ";
                    String.concat " " [Approximation.sizebound kind appr t v |> Bound.to_string;
                                       comp kind;
                                       RV.to_id_string (t, v)];
                    "\n";
                    "Local: ";
                    RV.to_string program_vars kind (t,v)]

let run (params: main_params) =
  let logs = List.map (fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;
  let input = Option.default_delayed read_line params.input in
  let input_filename =
    if params.simple_input then
      "dummyname"
    else
      input |> Fpath.v |> Fpath.normalize |> Fpath.rem_ext |> Fpath.filename
  and output_dir =
    Option.map Fpath.v params.output_dir
    |? (if params.simple_input then
          Fpath.v "."
        else
          input |> Fpath.v |> Fpath.parent)
  in
  if params.print_input then (
    let program_str =
      if params.simple_input then
        input
      else
        input |> File.lines_of |> List.of_enum |> String.concat "\n"
    in
    print_string (program_str ^ "\n\n")
  );
  input
  |> MainUtil.read_input params.simple_input
  |> Option.map (fun program ->
         (program, Approximation.create program)
         |> Preprocessor.process params.preprocessing_strategy params.preprocessors
         |> tap (fun (program, appr) ->
                if params.print_system then
                  Program.print_system ~label:TransitionLabel.to_string ~outdir:output_dir ~file:input_filename program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  Program.print_rvg `Lower ~label:(RV.to_string (Program.vars program) `Lower) ~outdir:output_dir ~file:input_filename program;
                  Program.print_rvg `Upper ~label:(RV.to_string (Program.vars program) `Upper) ~outdir:output_dir ~file:input_filename program
                )
              )
         |> (fun (program, appr) ->
                   if not params.no_boundsearch then
                     (program, appr)
                     |> uncurry Bounds.find_bounds
                     |> fun appr -> (program, appr)
                   else (program, appr))
         |> tap (fun (program, appr) -> params.result program appr)
         |> tap (fun (program, appr) ->
                if params.print_system then
                  Program.print_system ~label:(bounded_label_to_string appr) ~outdir:output_dir ~file:input_filename program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  Program.print_rvg `Lower ~label:(bounded_rv_to_string (Program.vars program) `Lower appr) ~outdir:output_dir ~file:input_filename program;
                  Program.print_rvg `Upper ~label:(bounded_rv_to_string (Program.vars program) `Upper appr) ~outdir:output_dir ~file:input_filename program;
                )
              )
       )
  |> ignore

let subcommand run params_cmdliner_term description command =
  Cmdliner.Term.(const run $ params_cmdliner_term (), info ~doc:description command)
  
let subcommands =
  [
    PrfCommand.(subcommand run params_cmdliner_term description command);
    LocalSizeBoundCommand.(subcommand run params_cmdliner_term description command);
    SizeCommand.(subcommand run params_cmdliner_term description command);
    SMTCommand.(subcommand run params_cmdliner_term description command);
    NormalizeCommand.(subcommand run params_cmdliner_term description command);
  ]
  
let () =
  (* Read the arguments from the shell via an api and call run *)
  let main_command = Cmdliner.Term.(const run $ main_params_cmdliner_term (), info Sys.argv.(0)) in
  (* Logger.init ["lsb", Logger.DEBUG; "size", Logger.DEBUG; "prf", Logger.DEBUG] (Logger.make_dbg_formatter IO.stdout); *)
  Cmdliner.Term.(exit @@ eval_choice main_command subcommands)

