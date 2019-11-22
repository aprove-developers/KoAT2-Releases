open Batteries

module Valuation = Valuation.Make(OurInt)

let description = "Find solutions for a constraint"

let command = "smt"

type params = {

    constr : string; [@pos 0]
    (** The constraint for which a solution should be found. *)

    solver : [`Z3]; [@enum ["z3", `Z3]] [@default `Z3]
    (** The solver which should be used. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  let input = "test.koat" in
  Logging.use_loggers [Logging.Preprocessor, Logger.DEBUG];
  MainUtil.read_input ~rename:false false "test.koat"
  |> Option.map (fun program ->
       (program, Approximation.create program)
       |> Preprocessor.process Preprocessor.process_only_once [Preprocessor.InvariantGeneration]
       |> tap (fun (program, appr) ->
            GraphPrint.print_system ~label:TransitionLabel.to_string
            ~outdir:(input |> Fpath.v |> Fpath.parent) ~file:"test" program))
  |> const @@ Printf.printf "Ende\n"