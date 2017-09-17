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
type params = {
    
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
               
let preprocessors: (Program_.t -> Program_.t) list = []

(* We apply each preprocessor exactly one time *)
let preprocess (graph: Program_.t): Program_.t =
  List.fold_left (fun graph preprocessor -> preprocessor graph) graph preprocessors

let find_bounds (graph: Program_.t): Approximation_.t =
  raise (Failure "Not yet implemented")

let print_results (appr: Approximation_.t): unit =
  raise (Failure "Not yet implemented")  

let run (params: params) =
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
  
let () =
  (* Read the arguments from the shell via an api and call run *)
  let open Cmdliner in
  let term = Term.(const run $ params_cmdliner_term ()) in
  let info = Term.info Sys.argv.(0) in
  Term.exit @@ Term.eval (term, info)

