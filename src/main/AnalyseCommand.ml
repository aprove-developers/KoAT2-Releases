open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Parameter

let command = "analyse"

let description = "Proceed a full time, cost and size analysis on a given integer transition system. Probabilistic and Nonprobabilistic Methods are supported."

let run (params: params) =
  let logs = List.map (fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;
  
  let input = Option.default_delayed read_line params.input in
  let input_filename =
    if params.simple_input then
      "dummyname"
    else
      input |> Fpath.v |> Fpath.normalize (*|> Fpath.rem_ext*) |> Fpath.to_string
      
  in
  let goal = MainUtil.read_goal params.simple_input input_filename in
  
  match goal with
    |"COMPLEXITY" -> DeterministicAnalysis.run params
    |"EXPECTEDCOMPLEXITY" -> ProbabilisticAnalysis.run params
    |"EXACTRUNTIME" -> ExactRuntime.run params
    | _ -> print_string ("The GOAL: "^goal^" you entered is not supported.\nKoAT2 supports \nCOMPLEXITY \nEXPECTEDCOMPLEXITY \nEXACT.\n");
  
