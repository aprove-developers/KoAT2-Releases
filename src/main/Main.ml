open Batteries

module Var_ = ID.StringID
module Value_ = PolyTypes.OurInt
module Polynomial_ = Polynomials.Make(Var_)(Value_)
module Constraint_ = Constraints.Make(Polynomial_)
module Program_ = Program.Make(Constraint_)
module Approximation_ = Approximation.Make(Program_)

module SMT_ = SMT.MakeZ3Solver(Constraint_)                      

module Reader_ = Readers.Make(Program_)

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
    
    output_dir : string; [@default "."]
    (** An absolute or relative path to the output directory, where all generated files should end up *)

  } [@@deriving cmdliner, show]

type localsizebound_params = {

    kind : [`Upper | `Lower]; [@enum ["upper", `Upper; "lower", `Lower]] [@pos 0]
    (** Which type of bound is requested. Available options: upper and lower. *)

    guard : string;
    (** The guard of the transition in the form of a constraint.
        That is a &&-separated list of atoms.
        Atoms are two polynomials in a relation with <, >, <=, >= or =. *)

    var : string; [@default "x"]
    (** The variable for which a local size bound should be found. *)

    update : string option;
    (** The polynomial to which the value of the variable gets updated after the transition. *)
    
  } [@@deriving cmdliner, show]

type smt_params = {

    constr : string; [@pos 0]
    (** The constraint for which a solution should be found. *)

    solver : [`Z3]; [@enum ["z3", `Z3]] [@default `Z3]
    (** The solver which should be used. *)

  } [@@deriving cmdliner, show]
                           
let preprocessors: (Program_.t -> Program_.t) list = []
                                                   
(* We apply each preprocessor exactly one time *)
let preprocess (graph: Program_.t): Program_.t =
  List.fold_left (fun graph preprocessor -> preprocessor graph) graph preprocessors

let find_bounds (graph: Program_.t): Approximation_.t =
  raise (Failure "Not yet implemented")

let print_results (appr: Approximation_.t): unit =
  raise (Failure "Not yet implemented")  

let run (params: main_params) =
  let program = Reader_.read_file params.input in
  if params.print_system then
    Program_.print_system ~outdir:params.output_dir ~file:"tmp" program;
  if params.print_rvg then
    Program_.print_rvg ~outdir:params.output_dir ~file:"tmp" program;
  if not params.no_boundsearch then
       program
    |> preprocess
    |> find_bounds
       |> print_results

let run_localsizebound (params: localsizebound_params) =
  let open Program_.TransitionLabel in
  let kind = match params.kind with
    | `Upper -> Upper
    | `Lower -> Lower in
  let guard = Reader_.read_constraint params.guard in
  let var = Polynomial_.Var.of_string params.var in
  let update = match params.update with
    | Some str -> Map.(add var (Reader_.read_polynomial str) empty)
    | None -> Map.empty in
  let label = make ~name:"" ~start:"" ~target:"" ~update ~guard in
  print_string (Bound.to_string (sizebound_local kind label var))

let run_smt (params: smt_params) =
  let module Z3 = SMT.MakeZ3Solver(Constraint_) in
  let solve = match params.solver with
    | `Z3 -> Z3.get_model
  and constr = Reader_.read_constraint params.constr in
     Polynomial_.Valuation_.bindings (solve constr)
  |> Enum.iter (fun (var,value) -> print_string (Polynomial_.Var.to_string var ^ " -> " ^ Polynomial_.Value.to_string value))
  
let subcommands =
    let open Cmdliner in [
        Term.(const run_localsizebound $ localsizebound_params_cmdliner_term (), Term.info ~doc:"Search for a local size bound" "lsb");
        Term.(const run_smt $ smt_params_cmdliner_term (), Term.info ~doc:"Find solutions for a constraint" "smt");
        ]
  
let () =
  (* Read the arguments from the shell via an api and call run *)
  let open Cmdliner in
  let main_command = (Term.(const run $ main_params_cmdliner_term ()), Term.info Sys.argv.(0)) in
  Term.exit @@ Term.eval_choice main_command subcommands

