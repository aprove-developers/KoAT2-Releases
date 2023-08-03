(** Handles shell arguments and computes for a guard and a specified variable a lower/upper local size-bound. *)
open Koat2
open OurBase
open Polynomials
open Formulas
open Bounds

let description = "Search for a local size bound"

let command = "lsb"

type params = {

    kind : [`Lower | `Upper]; [@enum ["upper", `Upper; "lower", `Lower]] [@pos 0] [@default `Upper]
    (** Which type of bound is requested. Available options: upper and lower. *)

    guard : string; [@default ""]
    (** The guard of the transition in the form of a constraint.
        That is a formula with and-separators (&&) and or-separators (||).
        Atoms are two polynomials in a relation with <, >, <=, >= or =. *)

    var : string; [@default "x"]
    (** The variable for which a local size bound should be found. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  Logging.(use_loggers [LocalSizeBound, Logger.DEBUG]);
  let open ProgramModules.TransitionLabel in
  let guard = Readers.read_formula params.guard in
  let var = Var.of_string params.var in
  print_string (Bound.to_string LocalSizeBound.(find_bound (Set.inter (Formula.vars guard) (VarSet.singleton var)) var guard 1024 |> Option.map ~f:Tuple2.first |> option_lsb_as_bound))
