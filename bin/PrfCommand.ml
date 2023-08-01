(** Handles shell arguments and computes ranking functions for a program. *)
open Koat2
open OurBase
open ProgramModules
open Readers

let description = "Search for a linear ranking function"

let command = "prf"

type params = {
    input : string; [@aka ["i"]] [@pos 0]
    (** Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)

    simple_input : bool; [@default false] [@aka ["s"]]
    (** If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  Logging.(use_loggers [PRF, Logger.DEBUG]);
  let depth = 1 in
  params.input
  |> Readers.read_input params.simple_input
  |> (fun program ->
         Approximation.create program
         |> TrivialTimeBounds.compute program
         |> (fun appr ->
                   let transitions =
                     program
                     |> Program.graph
                     |> TransitionGraph.transitions
                     |> Base.Set.filter ~f:(not % Approximation.is_time_bounded appr)
                   in
                   MultiphaseRankingFunction.find `Time program depth
                   |> Base.Sequence.filter ~f:(Base.Set.mem transitions % MultiphaseRankingFunction.decreasing)
                   |> Base.Sequence.iter ~f:(fun prf -> print_string (MultiphaseRankingFunction.to_string prf ^ "\n"))))
