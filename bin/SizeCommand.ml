open Koat2
(** Handles shell arguments and computes a size-bounds for a program. *)

open OurBase
open ProgramModules

let description = "Run a size bound improvement step"
let command = "size"

type params = {
  program : string; [@pos 0] [@docv "FILE"]  (** The file of the program which should be analyzed. *)
}
[@@deriving cmdliner, show]

module RVG = RVGTypes.MakeRVG (ProgramModules)
module LSB = LocalSizeBound.Make (TransitionLabel) (Transition) (Program)

let run (params : params) =
  Logging.(use_loggers [ (Size, Logger.DEBUG) ]);
  let appr = Approximation.empty and program = Readers.read_file params.program in
  let input_vars = Program.input_vars program in

  let lsbs =
    List.cartesian_product (Set.to_list @@ Program.transitions program) (Set.to_list input_vars)
    |> List.map ~f:(fun (t, v) -> ((t, v), LSB.compute_bound input_vars t v))
    |> Hashtbl.of_alist_exn (module ProgramModules.RV)
  in

  SizeBounds.improve program
    (RVG.rvg_with_sccs (Option.map ~f:(LSB.vars % Tuple2.first) % Hashtbl.find_exn lsbs) program)
    (Hashtbl.find_exn lsbs) appr
  |> Approximation.to_string program |> print_string
