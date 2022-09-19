(** Handles shell arguments and computes a size-bounds for a program. *)
open Batteries
open Koat2

let description = "Run a size bound improvement step"

let command = "size"

type params = {

    program : string; [@pos 0] [@docv "FILE"]
    (** The file of the program which should be analyzed. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  Logging.(use_loggers [Size, Logger.DEBUG]);
  let appr = Approximation.empty 10 3
  and program = Readers.read_file params.program in
  SizeBounds.improve program (RVGTypes.RVG.rvg_with_sccs program) appr
  |> Approximation.to_string program
  |> print_string

