open Batteries
open ProgramTypes
open Sys
open Unix
open Parameter
open ExactProgramTypes
open BoundsInst
(*let description = "Testing for exactRuntime"

let command = "exact"

type params = {
  in_file : string; [@aka ["i"]]
  (** path to input file *)
  logging : bool; [@default false] [@aka ["l"]]
  (** enable logging *)
} [@@deriving cmdliner, show]*)

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

let get_koat_path (command: String.t) = 
  let which_output = read_process_lines ("which " ^ command) |> String.concat "" in
  (* Returns nothing when it is called as the executable at the current working directory *)
  if String.is_empty which_output then
    Filename.current_dir_name
  (* Else a path to the koatP executable is given whether it is the 
  relative path or the total path. Though it is handled the same way
  I have split it up for now in case it has to be changed later *)
  else if Char.equal which_output.[0] '/' then
    Filename.dirname which_output
  else Filename.dirname which_output

let tap_option (f: 'a -> unit) (a: 'a option) : 'a option =
  Option.may f a;
  a

module ExactResult =
struct
  type t = {
    string_res: string option;
    warning: string option;
    error: string option;
    time: string option;
    evaluation: string option;
    bound: ExactBound.t option;
    lower: RealBound.t option;
    upper: RealBound.t option;
  }
  let bound res = res.bound
  let lower res = res.lower
  let upper res = res.upper

  let check_exn res =
    Option.map (fun str -> raise (ExactProgramTypes.Invalid_input str)) res.error
    |> ignore
  
  let rec get_from_list_ = function
    | (search, key :: value :: tail) -> if String.equal search key then Some value else get_from_list_ (search, tail)
    | (_, x :: []) -> None
    | (_, []) -> None

  let get_from_list search list =
    get_from_list_ (search, list)

  let from_list list = {
      string_res = get_from_list "STRING" list;
      warning = get_from_list "WARNING" list;
      error = get_from_list "ERROR" list;
      time = list |> get_from_list "TIME" ;
      evaluation = list |> get_from_list "EVALUATION" ;
      bound = list |> get_from_list "TREE" |> Option.map ExactReader.from_tree;
      lower = list |> get_from_list "LOWER" |> Option.map ExactReader.from_tree |> Option.map ExactBound.force_bound;
      upper = list |> get_from_list "UPPER" |> Option.map ExactReader.from_tree |> Option.map ExactBound.force_bound;
    }

  let to_string_ label value =
    value |> Option.map (fun str -> label ^ ": " ^ str) |> Option.map (List.make 1) |? []
  let to_string_list res =
    to_string_ "RESULT" res.string_res
    @ to_string_ "WARNING" res.warning
    @ to_string_ "ERROR" res.error
    @ to_string_ "TIME" res.time
    @ to_string_ "EVALUATION" res.evaluation
    @ to_string_ "LOWER BOUND" (Option.map (RealBound.show ~complexity:false) res.lower)
    @ to_string_ "UPPER BOUND" (Option.map (RealBound.show ~complexity:false) res.upper)

  let to_string res =
    res |> to_string_list |> String.concat "\n"

end


let run (params: params) =
  (* let logs = List.map (fun log -> (log, params.log_level)) params.logs in
    Logging.use_loggers logs; *)
  let logger = Logging.(get ExactRuntime) in
  let input = Option.default_delayed read_line params.input in
  let input_filename = input |> Fpath.v |> Fpath.normalize |> Fpath.to_string in
  let output_dir = params.output_dir in

  let print_result result =
    result |> ExactResult.to_string |> print_string; print_string "\n"
  in

  let write_result result =
    let out_name = 
      (input |> Fpath.v |> Fpath.rem_ext |> Fpath.filename) ^ ".result" 
    in
    output_dir
    |> Option.map (fun out_dir ->
        result
        |> ExactResult.to_string_list
        |> List.enum
        |> File.write_lines (out_dir ^ out_name) 
      )
    |> ignore;
  in

  let execute () =
    let sage_path = get_koat_path Sys.argv.(0) ^ "/../exactruntime/exact_runtime_from_koat.py" in
    input_filename
    |> Readers.read_exact_file
    |> (fun ep -> if ExactProgram.is_valid ~logger:logger ep then Some ep else None)
    |> Option.map (fun ep -> 
                      ep
                      |> ExactProgram.to_sage
                      |> fun args -> "python3 " ^ sage_path ^ " " ^ args
                      |> read_process_lines
                      |> ExactResult.from_list
                      |> tap(ExactResult.check_exn)
                  )
    |> tap_option print_result
    |> tap_option write_result
    |> Option.map ExactResult.bound
    |> fun bound_o -> if Option.is_some bound_o then Option.get bound_o else None
  in
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "Calculating exact runtime.", [])
                  ~result:(fun res ->
                    res |? ExactBound.infinity
                    |> ExactBound.to_string
                  )
                  execute
  |> ignore
