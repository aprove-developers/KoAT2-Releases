open Batteries
open ProgramTypes
open Sys
open Unix
let description = "Testing for exactRuntime"

let command = "exact"

type params = {
    m : string; [@pos 0] [@aka ["m"]]
    (** biggest increase in the variable *)
    k : string; [@pos 1]
    (** biggest decrease in the variable *) 
    p' : string; [@pos 2]
    (** Probability to set the variable to 0. *)
    probs : string list; [@pos 3] [@sep ',']
    (** Probabilities for the program, starting at the top*)
    precision : int; [@default 100] [@aka ["prec"]]
    (** Bit precision for Calculations in Sage Math *)
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
  let logger = Logging.(get ExactRuntime) in
  let execute () =
    (* No idea if this actaully works, may need adjustment in the future *)
    let sage_path = 
    Sys.argv.(0)
    |> Filename.dirname
    |> fun koat_path -> koat_path ^ "/../exact_runtime/exact_runtime.sage"
    in
    Filename.current_dir_name
    |> print_string;
    [[string_of_int params.precision; params.m; params.k; params.p']; params.probs]
    |> List.concat
    |> String.concat " "
    |> fun input -> read_process_lines (sage_path ^ " " ^ input)
    |> String.concat ""
    |> tap(fun string -> print_string (string ^ "\n"))
  in 
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "exact runtime", [])
                  ~result:(fun bound ->
                    bound
                  )
                  execute
  |> ignore