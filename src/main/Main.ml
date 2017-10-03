open Batteries

module Value = PolyTypes.OurInt
module Formula = Formula.PolynomialFormula
module Constraint = Constraints.PolynomialConstraint
module Atom = Atoms.PolynomialAtom
module Valuation = Valuation.Make(PolyTypes.OurInt)
            
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

type normalize_params = {

    kind : [`Atom | `Polynomial | `Bound]; [@enum ["atom", `Atom; "poly", `Polynomial; "bound", `Bound]] [@pos 0]  [@docv "KIND"]
    (** How the input should be interpreted. *)

    input : string; [@pos 1] [@docv "INPUT"]
    (** The input which should be normalized *)
    
  } [@@deriving cmdliner, show]
                
let preprocessors: (Program.t -> Program.t) list = []
                                                   
(* We apply each preprocessor exactly one time *)
let preprocess (graph: Program.t): Program.t =
  List.fold_left (fun graph preprocessor -> preprocessor graph) graph preprocessors

let find_bounds (graph: Program.t): Approximation.t =
  raise (Failure "Not yet implemented")

let print_results (appr: Approximation.t): unit =
  raise (Failure "Not yet implemented")  

let run (params: main_params) =
  let program = Readers.read_file params.input in
  if params.print_system then
    Program.print_system ~outdir:params.output_dir ~file:"tmp" program;
  if params.print_rvg then
    Program.print_rvg ~outdir:params.output_dir ~file:"tmp" program;
  if not params.no_boundsearch then
       program
    |> preprocess
    |> find_bounds
       |> print_results

let run_localsizebound (params: localsizebound_params) =
  let open TransitionLabel in
  let kind = match params.kind with
    | `Upper -> Upper
    | `Lower -> Lower in
  let guard = Readers.read_constraint params.guard in
  let var = Var.of_string params.var in
  let update = match params.update with
    | Some str -> Map.(add var (Readers.read_polynomial str) empty)
    | None -> Map.empty in
  let label = make ~name:"" ~start:"" ~target:"" ~update ~guard in
  print_string (Bound.to_string LocalSizeBound.(as_bound (sizebound_local kind label var)))

let run_smt (params: smt_params) =
  let module Z3 = SMT.Z3Solver in
  let solve = match params.solver with
    | `Z3 -> Z3.get_model
  and constr = Readers.read_formula params.constr in
  let valuation_bindings = Valuation.bindings (solve constr) in
  if Enum.is_empty valuation_bindings then
    print_string "unsatisfiable"
  else Enum.iter (fun (var,value) -> print_string (Var.to_string var ^ " -> " ^ Value.to_string value ^ "\n")) valuation_bindings
  
let run_normalize (params: normalize_params) =
  let output = match params.kind with
    | `Polynomial -> Polynomial.to_string (Polynomial.simplify (Readers.read_polynomial params.input))
    | `Atom -> Atom.to_string (Readers.read_atom params.input)
    | `Bound -> Bound.to_string (Readers.read_bound params.input) in
  print_string output
  
let subcommands =
  let open Cmdliner in [
      Term.(const run_localsizebound $ localsizebound_params_cmdliner_term (), Term.info ~doc:"Search for a local size bound" "lsb");
      Term.(const run_smt $ smt_params_cmdliner_term (), Term.info ~doc:"Find solutions for a constraint" "smt");
      Term.(const run_normalize $ normalize_params_cmdliner_term (), Term.info ~doc:"Find a normalform for an input" "normalize");
    ]
  
let () =
  (* Read the arguments from the shell via an api and call run *)
  let open Cmdliner in
  let main_command = (Term.(const run $ main_params_cmdliner_term ()), Term.info Sys.argv.(0)) in
  Logger.init ["lsb", Logger.DEBUG] (Logger.make_dbg_formatter IO.stdout);
  Term.exit @@ Term.eval_choice main_command subcommands

