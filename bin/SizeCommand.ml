(** Handles shell arguments and computes a size-bounds for a program. *)
open Batteries
open Koat2
open ProgramModules

let description = "Run a size bound improvement step"

let command = "size"

type params = {

    program : string; [@pos 0] [@docv "FILE"]
    (** The file of the program which should be analyzed. *)

  } [@@deriving cmdliner, show]

module RVG = RVGTypes.MakeRVG(ProgramModules)
module LSB = LocalSizeBound.Make(TransitionLabel)(Transition)(Program)
module LSB_Table = Hashtbl.Make(ProgramModules.RV.RVTuple_)

let run (params: params) =
  Logging.(use_loggers [Size, Logger.DEBUG]);
  let appr = Approximation.empty 10 3
  and program = Readers.read_file params.program in
  let input_vars = Program.input_vars program in

  let lsbs =
    List.cartesian_product
      (TransitionSet.to_list @@ Program.transitions program)
      (VarSet.to_list input_vars)
    |> List.enum
    |> Enum.map (fun(t,v) -> (t,v),LSB.compute_bound input_vars t v)
    |> LSB_Table.of_enum
  in


  SizeBounds.improve program
    (RVG.rvg_with_sccs (Option.map (LSB.vars % Tuple2.first)% LSB_Table.find lsbs) program)
    (LSB_Table.find lsbs) appr
  |> Approximation.to_string program
  |> print_string

