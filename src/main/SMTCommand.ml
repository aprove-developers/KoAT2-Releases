open Batteries
open BoundsInst

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
  let b =
    Readers.read_bound "inf * X"
  in
  Logging.use_loggers [Logging.Bound, Logger.DEBUG];
  Printf.printf "Hello World %s\n" (Bound.to_string (Bound.simplify_vars_nonnegative b))