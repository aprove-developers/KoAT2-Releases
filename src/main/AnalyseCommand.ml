open Batteries
open ProgramTypes
open RVGTypes

let command = "analyse"

let description = "Proceed a full time, cost and size analysis on a given integer transition system"

(** Prints the whole resulting approximation to the shell. *)
let print_all_bounds (program: Program.t) (appr: Approximation.t): unit =
  print_string (Approximation.to_string program appr)

(** Prints the overall timebound of the program to the shell. *)
let print_overall_timebound (program: Program.t) (appr: Approximation.t): unit =
  program
  |> Approximation.program_timebound appr
  |> Bound.to_string
  |> print_endline

(** Prints the overall timebound of the program to the shell. *)
let print_termcomp (program: Program.t) (appr: Approximation.t): unit =
  program
  |> Approximation.program_costbound appr
  |> Bound.asymptotic_complexity
  |> Bound.show_complexity_termcomp
  |> print_endline

(** The shell arguments which can be defined in the console. *)
type params = {

    print_system : bool;
    (* Prints the integer transition system at the start as png *)

    print_rvg : bool;
    (* Prints the input result variable graph at the start as png *)

    print_input : bool;
    (* Prints the raw unmodified input before the start *)

    no_boundsearch : bool; [@aka ["n"]]
    (* Disables the search for bounds. Useful if you just want information about the integer transition system via the other options or for debugging purposes. *)

    input : string option; [@aka ["i"]]
    (* Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)

    simple_input : bool; [@default false] [@aka ["s"]]
    (* If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)
    
    output_dir : string option; [@aka ["o"]]
    (* An absolute or relative path to the output directory, where all generated files should end up. *)

    logs : Logging.logger list; [@enum Logging.(List.map (fun l -> show_logger l, l) loggers)] [@default Logging.all] [@sep ','] [@aka ["l"]]
    (* The loggers which should be activated. *)

    log_level : Logger.level; [@enum Logger.([NONE; FATAL; ERROR; WARN; NOTICE; INFO; DEBUG]) |> List.map (fun level -> Logger.name_of_level level, level)] [@default Logger.NONE]
    (* The general log level of the loggers. *)
    
    result : (Program.t -> Approximation.t -> unit); [@enum ["termcomp", print_termcomp; "all", print_all_bounds; "overall", print_overall_timebound]] [@default print_overall_timebound] [@aka ["r"]]
    (* The kind of output which is deserved. The option "all" prints all time- and sizebounds found in the whole program, the option "overall" prints only the sum of all timebounds. The option "termcomp" prints the approximated complexity class. *)
    
    preprocessors : Preprocessor.t list; [@enum Preprocessor.(List.map (fun p -> show p, p) all)] [@default Preprocessor.([InvariantGeneration; CutUnsatisfiableTransitions; CutUnreachableLocations])]
    (* The preprocessors which should be applied before running the actual algorithm. *)
    
    preprocessing_strategy : Preprocessor.strategy; [@enum Preprocessor.["once", process_only_once; "fixpoint", process_til_fixpoint]] [@default Preprocessor.process_til_fixpoint]
    (* The strategy which should be used to apply the preprocessors. *)

    rename : bool; [@default false]
    (* If the location names should be normalized to simplified names. *)

  } [@@deriving cmdliner]

let bounded_label_to_string (appr: Approximation.t) (label: TransitionLabel.t): string =
  String.concat "" ["Timebound: ";
                    Approximation.timebound_id appr (TransitionLabel.id label) |> Bound.to_string;
                    "\n";
                    TransitionLabel.to_string label]

let bounded_rv_to_string (program: Program.t) kind (appr: Approximation.t) (t,v) =
  let get_lsb kind (t, v) =
    LocalSizeBound.(sizebound_local program kind t v |> Option.map as_bound |? default kind)
  in
  String.concat "" [RV.to_id_string (t, v);
                    "\n";
                    "Global: ";
                    Approximation.sizebound kind appr t v |> Bound.to_string;
                    "\n";
                    "Local: ";
                    get_lsb kind (t,v) |> Bound.show ~complexity:false
    ]
  
let get_lsb program kind (t, v) =
  LocalSizeBound.(sizebound_local program kind t v |> Option.map as_bound |? default kind)
  
let standard_vars program =
  let open Program in
  0
  |> TransitionGraph.fold_edges_e (fun edge size -> Int.max (TransitionLabel.input_size (Transition.label edge)) size) (graph program)
  |> Var.fresh_arg_list
  
(* For each transition rename standard_vars transition *)

let rename_graph standard_vars graph =
  let transitions = (TransitionSet.enum % TransitionGraph.transitions) graph in
    Enum.fold (fun program_graph transition -> TransitionGraph.replace_edge_e transition (Transition.rename standard_vars transition) program_graph) graph transitions

let rename_program program =
  let standard_vars = standard_vars program in
    Program.map_graph (rename_graph standard_vars) program

let rename_program_option opt =
  match opt with
    |Some program -> Some (rename_program program)
    |None -> None

let run (params: params) =
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
  |> MainUtil.read_input ~rename:params.rename params.simple_input
  |> rename_program_option
  |> Option.map (fun program ->
         (program, Approximation.create program)
         |> Preprocessor.process params.preprocessing_strategy params.preprocessors
         |> tap (fun (program, appr) ->
                if params.print_system then
                  GraphPrint.print_system ~label:TransitionLabel.to_string ~outdir:output_dir ~file:input_filename program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  GraphPrint.print_rvg `Lower ~label:RV.to_id_string ~outdir:output_dir ~file:input_filename program;
                  GraphPrint.print_rvg `Upper ~label:RV.to_id_string ~outdir:output_dir ~file:input_filename program
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
                  GraphPrint.print_system ~label:(bounded_label_to_string appr) ~outdir:output_dir ~file:input_filename program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  GraphPrint.print_rvg `Lower ~label:(bounded_rv_to_string program `Lower appr) ~outdir:output_dir ~file:input_filename program;
                  GraphPrint.print_rvg `Upper ~label:(bounded_rv_to_string program `Upper appr) ~outdir:output_dir ~file:input_filename program;
                )
              )
       )
  |> ignore
