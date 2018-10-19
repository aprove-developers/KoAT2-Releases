open Batteries
open ProgramTypes
open Sys
open Unix
let description = "Testing for exactRuntime"

let command = "exact"

type params = {
    input : string; [@aka ["i"]] [@pos 0]
    (** Polynomial in String form we want to get the roots from*)
    
  } [@@deriving cmdliner, show]

let read_process_lines command =
  let lines = ref [] in
  let in_channel = Unix.open_process_in command in
  begin
    try
      while true do
        lines := input_line in_channel :: !lines
      done;
    with 
      | BatInnerIO.Input_closed -> ()
      | End_of_file -> ()
  end;
  List.rev !lines

let run (params: params) =
  Logging.(use_loggers [ExactRuntime, Logger.DEBUG]); 
  let logger = Logging.(get Roots) in
  let execute () =
    params.input
    |> fun input -> read_process_lines ("sage src/exact_runtime/exact_runtime.sage " ^ input)
    |> String.concat ""
  in 
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find roots", [])
                  ~result:(fun bound ->
                    bound
                  )
                  execute
  |> ignore