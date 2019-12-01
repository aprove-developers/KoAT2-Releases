open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Parameter

let command = "analyse"

let description = "Proceed a full time, cost and size analysis on a given integer transition system. Probabilistic and Nonprobabilistic Methods are supported."

let supported_goals = ["COMPLEXITY"; "EXPECTEDCOMPLEXITY"; "EXACTRUNTIME"]

let goal_support (goal:string) =
  let goal_map =
    [("COMPLEXITY", DeterministicAnalysis.run); ("EXPECTEDCOMPLEXITY", ProbabilisticAnalysis.run); ("EXACTRUNTIME", ExactRuntime.run);]
    |> List.enum
    |> Map.of_enum
  in
    Map.find goal goal_map


let run (params: params) =
  let logs = List.map (fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;

  let input = Option.default_delayed read_line params.input in
  let input_filename =
    if params.simple_input then
      "dummyname"
    else
      input |> Fpath.v |> Fpath.normalize |> Fpath.to_string
  in
  let goal = MainUtil.read_goal params.simple_input input_filename in
    try
      goal_support goal params
    with
      Not_found -> print_string ("The GOAL: " ^ goal ^ " you entered is not supported.\nKoAT2 supports\n" ^ (String.concat "\n" supported_goals) ^ "\n")
