open Batteries
open Program.Types
   
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
  params.input
  |> MainUtil.read_input params.simple_input
  |> Option.may (fun program ->
         (program, Approximation.create program)
         |> TrivialTimeBounds.transform
         |> MaybeChanged.unpack
         |> (fun (program, appr) ->
                   let transitions =
                     program
                     |> Program.graph
                     |> TransitionGraph.transitions
                     |> TransitionSet.to_list
                     |> List.filter (Bound.is_infinity % Approximation.timebound appr)
                   in
                   RankingFunction.find (Program.vars program) transitions
                   |> (fun prf -> print_string (RankingFunction.to_string prf ^ "\n"))))
