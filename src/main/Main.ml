open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
module Valuation = Valuation.Make(OurInt)
            
module SMT_ = SMT.Z3Solver                      

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

type localsizebound_params = {

    kind : [`Upper | `Lower]; [@enum ["upper", `Upper; "lower", `Lower]] [@pos 0] [@default `Upper]
    (** Which type of bound is requested. Available options: upper and lower. *)

    guard : string; [@default ""]
    (** The guard of the transition in the form of a constraint.
        That is a &&-separated list of atoms.
        Atoms are two polynomials in a relation with <, >, <=, >= or =. *)

    var : string; [@default "x"]
    (** The variable for which a local size bound should be found. *)

    update : string option;
    (** The polynomial to which the value of the variable gets updated after the transition. *)
    
  } [@@deriving cmdliner, show]
  
type prf_params = {
  input : string; [@pos 0] [@docv "INPUT"]
  (** An absolute or relative path to the koat input file which defines the integer transition system *)
  } [@@deriving cmdliner, show]
  
type smt_params = {

    constr : string; [@pos 0]
    (** The constraint for which a solution should be found. *)

    solver : [`Z3]; [@enum ["z3", `Z3]] [@default `Z3]
    (** The solver which should be used. *)

  } [@@deriving cmdliner, show]

type normalize_params = {

    kind : [`Atom | `Polynomial | `Bound]; [@enum ["atom", `Atom; "poly", `Polynomial; "bound", `Bound]] [@pos 0]  [@docv "KIND"]
    (** How the input should be interpreted. *)

    input : string; [@pos 1] [@docv "INPUT"]
    (** The input which should be normalized *)
    
  } [@@deriving cmdliner, show]

type size_params = {

    program : string; [@pos 0] [@docv "FILE"]
    (** The file of the program which should be analyzed. *)

  } [@@deriving cmdliner, show]

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
  |> (fun (program, appr) ->
    if params.print_system then
      Program.print_system ~outdir:output_dir ~file:input_filename program;
    (program, appr))
  |> (fun (program, appr) ->
    if params.print_rvg then
      Program.print_rvg ~outdir:output_dir ~file:input_filename program;
    (program, appr))
  |> fun (program, appr) ->
     if not params.no_boundsearch then
       (program, appr)
       |> params.preprocessing_strategy params.preprocessors
       |> uncurry Bounds.find_bounds
       |> print_results program

let run_localsizebound (params: localsizebound_params) =
  init_logger ["lsb", Logger.DEBUG];
  let open TransitionLabel in
  let guard = Readers.read_constraint params.guard in
  let var = Var.of_string params.var in
  let update = match params.update with
    | Some str -> VarMap.(add var (Readers.read_polynomial str) empty)
    | None -> VarMap.empty in
  let label = make "" ~start:"" ~target:"" ~update ~guard in
  print_string (Bound.to_string LocalSizeBound.(as_bound (sizebound_local params.kind label var)))

let run_prf_search (params: prf_params) =
  let program = Readers.read_file params.input in
    let nb_vars = VarSet.cardinal (Program.vars program) in
      let nb_trans = Program.TransitionGraph.nb_edges (Program.graph program ) in
        let appr = Approximation.empty nb_trans nb_vars in
          let prf = RankingFunction.find program appr in
            print_string (RankingFunction.to_string prf)
  
let run_smt (params: smt_params) =
  let module Z3 = SMT.Z3Solver in
  let solve = match params.solver with
    | `Z3 -> Z3.get_model
  and constr = Readers.read_formula params.constr in
  let valuation_bindings = Valuation.bindings (solve constr) in
  if Enum.is_empty valuation_bindings then
    print_string "unsatisfiable\n"
  else Enum.iter (fun (var,value) -> print_string (Var.to_string var ^ " -> " ^ OurInt.to_string value ^ "\n")) valuation_bindings
  
let run_normalize (params: normalize_params) =
  let output = match params.kind with
    | `Polynomial -> Polynomial.to_string (Polynomial.simplify (Readers.read_polynomial params.input))
    | `Atom -> Atom.to_string (Readers.read_atom params.input)
    | `Bound -> Bound.to_string (Readers.read_bound params.input) in
  print_string output
  
let run_size (params: size_params) =
  init_logger ["size", Logger.DEBUG];
  let appr = Approximation.empty 10 3
  and program = Readers.read_file params.program in
  SizeBounds.improve program appr
  |> Approximation.to_string program
  |> print_string

let subcommands =
  let open Cmdliner in [
      Term.(const run_prf_search $ prf_params_cmdliner_term (), Term.info ~doc:"Search for a linear ranking function" "prf");
      Term.(const run_localsizebound $ localsizebound_params_cmdliner_term (), Term.info ~doc:"Search for a local size bound" "lsb");
      Term.(const run_size $ size_params_cmdliner_term (), Term.info ~doc:"Run a size bound improvement step" "size");
      Term.(const run_smt $ smt_params_cmdliner_term (), Term.info ~doc:"Find solutions for a constraint" "smt");
      Term.(const run_normalize $ normalize_params_cmdliner_term (), Term.info ~doc:"Find a normalform for an input" "normalize");
    ]
  
let () =
  (* Read the arguments from the shell via an api and call run *)
  let open Cmdliner in
  let main_command = (Term.(const run $ main_params_cmdliner_term ()), Term.info Sys.argv.(0)) in
  (* Logger.init ["lsb", Logger.DEBUG; "size", Logger.DEBUG; "prf", Logger.DEBUG] (Logger.make_dbg_formatter IO.stdout); *)
  Term.exit @@ Term.eval_choice main_command subcommands

