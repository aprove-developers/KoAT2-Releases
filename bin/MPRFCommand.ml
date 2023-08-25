open Koat2
open! OurBase
open ProgramModules

(** Handles shell arguments and computes ranking functions for a program. *)

let description = "Search for a multiphase linear ranking function"
let command = "mprf"

type params = {
  input : string; [@aka [ "i" ]] [@pos 0]
      (** Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)
  simple_input : bool; [@default false] [@aka [ "s" ]]
      (** If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)
  depth : int; [@default 1] [@aka [ "d" ]]
      (** The maximum depth of a Multiphase Ranking Function to bound search space.*)
}
[@@deriving cmdliner, show]

let run (params : params) =
  Logging.(use_loggers [ (MPRF, Logger.DEBUG) ]);
  params.input |> Readers.read_input params.simple_input |> fun program ->
  Approximation.empty |> TrivialTimeBounds.compute program |> fun appr ->
  let transitions =
    program |> Program.graph |> TransitionGraph.transitions
    |> Set.filter ~f:(not % Approximation.is_time_bounded appr)
  in
  MultiphaseRankingFunction.find `Time program params.depth
  |> Sequence.filter ~f:(Set.mem transitions % MultiphaseRankingFunction.decreasing)
  |> Sequence.iter ~f:(fun prf -> print_string (MultiphaseRankingFunction.to_string prf ^ "\n"))
