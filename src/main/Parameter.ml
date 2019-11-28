open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes


module RV = Make_RV(Transition)
module ERV = Make_RV(RVTransitions.TransitionForExpectedSize)


(** Prints the whole resulting approximation with the expected timebounds to the shell. *)
let print_all_expected_bounds (program: Program.t) (appr: Approximation.t): unit =
  print_string (Approximation.to_string program true appr)

(** Prints the only the deterministic bounds of the resulting approximation to the shell. *)
let print_all_deterministic_bounds (program: Program.t) (appr: Approximation.t): unit =
  print_string (Approximation.to_string program false appr)

(** Prints the overall expected timebound of the program to the shell. *)
let print_overall_expected_timebound (program: Program.t) (appr: Approximation.t): unit =
  program
  |> Approximation.program_exptimebound appr
  |> RealBound.to_string
  |> print_endline

(** Prints the overall expected costbound of the program to the shell. *)
let print_overall_expected_costbound (program: Program.t) (appr: Approximation.t): unit =
  program
  |> Approximation.program_expcostbound appr
  |> RealBound.to_string
  |> print_endline

(** Prints the overall timebound of the program to the shell. *)
let print_overall_deterministic_timebound (program: Program.t) (appr: Approximation.t): unit =
  program
  |> Approximation.program_timebound appr
  |> Bound.to_string
  |> print_endline

(** Prints the overall deterministic timebound of the program to the shell. *)
let print_termcomp_deterministic (program: Program.t) (appr: Approximation.t): unit =
  program
  |> Approximation.program_costbound appr
  |> Bound.asymptotic_complexity
  |> Bound.show_complexity_termcomp
  |> print_endline

(** Prints the overall expected timebound of the program to the shell. *)
let print_termcomp_expected (program: Program.t) (appr: Approximation.t): unit =
  program
  |> Approximation.program_exptimebound appr
  |> RealBound.asymptotic_complexity
  |> RealBound.show_complexity_termcomp
  |> print_endline

(** The shell arguments which can be defined in the console. *)
type params = {

    print_system : bool;
    (** Prints the integer transition system at the start as png *)

    print_rvg : bool;
    (** Prints the input result variable graph at the start as png *)

    print_ervg : bool;
    (** Prints the input general result variable graph at the start as png *)

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

    result : string; [@default "overall"] [@aka ["r"]]
    (** The kind of output which is deserved. The option "all" prints all time- and sizebounds found in the whole program, the option "overall" prints only the sum of all timebounds. The option "termcomp" prints the approximated complexity class. *)

    preprocessors : Preprocessor.t list; [@enum Preprocessor.(List.map (fun p -> show p, p) all)] [@default Preprocessor.([InvariantGeneration; CutUnsatisfiableTransitions; CutUnreachableLocations])]
    (** The preprocessors which should be applied before running the actual algorithm. *)

    preprocessing_strategy : Preprocessor.strategy; [@enum Preprocessor.["once", process_only_once; "fixpoint", process_til_fixpoint]] [@default Preprocessor.process_til_fixpoint]
    (** The strategy which should be used to apply the preprocessors. *)

    rename : bool; [@default false]
    (** If the location names should be normalized to simplified names. *)

  } [@@deriving cmdliner]

let bounded_label_to_string (appr: Approximation.t) (label: TransitionLabel.t): string =
  String.concat "" ["Timebound: ";
                    Approximation.timebound_id appr (TransitionLabel.id label) |> Bound.to_string;
                    "\n";
                    TransitionLabel.to_string label]

let bounded_rv_to_string cache (program: Program.t) kind (appr: Approximation.t) (t,v) =
  let get_lsb kind (t, v) =
    LocalSizeBound.(sizebound_local cache program kind t v |> Option.map as_bound |? default kind)
  in
  String.concat "" [RV.to_id_string (t, v);
                    "\n";
                    "Global: ";
                    Approximation.sizebound kind appr t v |> Bound.to_string;
                    "\n";
                    "Local: ";
                    get_lsb kind (t,v) |> Bound.show ~complexity:false
    ]

let bounded_erv_to_string elsb_cache (program: Program.t) (appr: Approximation.t) ((gt,l),v) =
  String.concat "" [ERV.to_id_string ((gt,l), v);
                    "\n";
                    "Global: ";
                    Approximation.expsizebound_abs appr (gt,l) v |> RealBound.to_string;
                    "\n";
                    "Local: ";
                    ExpLocalSizeBound.elsb elsb_cache program ((gt,l),v) |> RealBound.to_string
    ]

let get_lsb cache program kind (t, v) =
  LocalSizeBound.(sizebound_local program cache kind t v |> Option.map as_bound |? default kind)

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

let rename_program_option =
  Option.map rename_program
  (*match opt with
    |Some program -> Some (rename_program program)
    |None -> None*)
