open Batteries
open ProgramTypes

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
  let cache = CacheManager.new_cache () in

  Logging.(use_loggers [PRF, Logger.DEBUG]);
  params.input
  |> MainUtil.read_input (CacheManager.trans_id_counter cache) params.simple_input
  |> Option.may (fun program ->
         Approximation.create program
         |> TrivialTimeBounds.compute program
         |> (fun appr ->
                   let transitions =
                     program
                     |> Program.graph
                     |> TransitionGraph.transitions
                     |> TransitionSet.filter (not % Approximation.is_time_bounded appr)
                   in
                   transitions
                   |> TransitionSet.to_list
                   |> List.map (RankingFunction.find (CacheManager.ranking_cache cache) `Time program)
                   |> List.flatten
                   |> List.map (RankingFunction.to_string)
                   |> String.concat "\n"
                   |> (flip (^)) "\n"
                   |> print_string
                   ))
