open Batteries
open ProgramTypes
   
let description = "Testing for lexRSM functionality"

let command = "lexrsm"

type params = {
    input : string; [@aka ["i"]] [@pos 0]
    (** Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)

    simple_input : bool; [@default false] [@aka ["s"]]
    (** If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)
    
  } [@@deriving cmdliner, show]

let run (params: params) =
  Logging.(use_loggers [LexRSM, Logger.DEBUG]);
  params.input
  |> MainUtil.read_input_goal false
  |> (fun (program, goal) ->
        if Option.is_some goal then
        program
        |> Option.map (fun program -> (program, Approximation.create program))
        |> Option.map (Preprocessor.process Preprocessor.process_til_fixpoint Preprocessor.([InvariantGeneration; ProbabilityLessOne]))
        |> Option.map (fun (prog, appr) -> LexRSM.find prog (Option.get goal))
        |> ignore)