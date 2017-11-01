open Batteries
            
(** The shell arguments which can be defined in the console. *)
type main_params = {
    
    print_system : bool;
    (** Prints the integer transition system at the start as png *)

    print_rvg : bool;
    (** Prints the input result variable graph at the start as png *)

    no_boundsearch : bool;
    (** Disables the search for bounds. Useful if you just want information about the integer transition system via the other options or for debugging purposes. *)

    input : string; [@pos 0] [@docv "INPUT"]
    (** An absolute or relative path to the koat input file which defines the integer transition system *)
    
    output_dir : string option;
    (** An absolute or relative path to the output directory, where all generated files should end up *)

    logs : string list; [@default []] [@sep ',']
    (** The loggers which should be activated. *)

    preprocessors : Preprocessor.t list; [@enum Preprocessor.(List.map (fun p -> show p, p) all)] [@default Preprocessor.all]
    (** The preprocessors which should be applied before running the actual algorithm. *)
    
    preprocessing_strategy : Preprocessor.strategy; [@enum Preprocessor.["once", process_only_once; "fixpoint", process_til_fixpoint]] [@default Preprocessor.process_til_fixpoint]
    (** The strategy which should be used to apply the preprocessors. *)
    
  } [@@deriving cmdliner]

let print_results (program: Program.t) (appr: Approximation.t): unit =
  print_string (Approximation.to_string program appr)

let init_logger (logs: (string * Logger.level) list) =
  Logger.init logs (Logger.make_dbg_formatter IO.stdout)
  
let run (params: main_params) =
  let logs = List.map (fun log -> (log, Logger.DEBUG)) params.logs in
  init_logger logs;
  let input_filename =
    params.input |> Fpath.v |> Fpath.normalize |> Fpath.rem_ext |> Fpath.filename
  and output_dir =
    Option.map Fpath.v params.output_dir |? (params.input |> Fpath.v |> Fpath.parent)
  in
  (Readers.read_file params.input, Approximation.empty 10 10) (* TODO Better values *)
  |> params.preprocessing_strategy params.preprocessors
  |> tap (fun (program, appr) ->
    if params.print_system then
      Program.print_system ~outdir:output_dir ~file:input_filename program)
  |> tap (fun (program, appr) ->
    if params.print_rvg then
      Program.print_rvg ~outdir:output_dir ~file:input_filename program)
  |> fun (program, appr) ->
     if not params.no_boundsearch then
       (program, appr)
       |> params.preprocessing_strategy params.preprocessors
       |> uncurry Bounds.find_bounds
       |> print_results program

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

