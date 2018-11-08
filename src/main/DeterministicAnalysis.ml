open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Parameter

let run (params: params) =
  let logs = List.map (fun log -> (log, params.log_level)) params.logs in
  Logging.use_loggers logs;
  let input = Option.default_delayed read_line params.input in
  let input_filename =
    if params.simple_input then
      "dummyname"
    else
      input |> Fpath.v |> Fpath.normalize |> Fpath.rem_ext |> Fpath.filename
  and output_dir =
    Option.map Fpath.v params.output_dir
    |? (if params.simple_input then
          Fpath.v "."
        else
          input |> Fpath.v |> Fpath.parent)
  in
  if params.print_input then (
    let program_str =
      if params.simple_input then
        input
      else
        input |> File.lines_of |> List.of_enum |> String.concat "\n"
    in
    print_string (program_str ^ "\n\n")
 ) ;
  let result_print = 
    match params.result with
    |"termcomp" -> print_termcomp_deterministic
    |"all" -> print_all_deterministic_bounds
    |_ -> print_overall_deterministic_timebound
  in
  
  input
  |>MainUtil.read_input ~rename:params.rename params.simple_input
  |> rename_program_option
  |> Option.map (fun program ->
         (program, Approximation.create program)
         |> Preprocessor.process params.preprocessing_strategy params.preprocessors
         |> tap (fun (program, appr) ->
                if params.print_system then
                  GraphPrint.print_system ~label:TransitionLabel.to_string ~outdir:output_dir ~file:input_filename program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  GraphPrint.print_rvg `Lower ~label:RV.to_id_string ~outdir:output_dir ~file:input_filename program;
                  GraphPrint.print_rvg `Upper ~label:RV.to_id_string ~outdir:output_dir ~file:input_filename program
                )
              )
         |> (fun (program, appr) ->
                   if not params.no_boundsearch then
                     (program, appr)
                     |> uncurry Bounds.find_bounds
                     |> fun appr -> (program, appr)
                   else (program, appr))
         |> tap (fun (program, appr) -> result_print program appr)
         |> tap (fun (program, appr) ->
                if params.print_system then
                  GraphPrint.print_system ~label:(bounded_label_to_string appr) ~outdir:output_dir ~file:input_filename program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  GraphPrint.print_rvg `Lower ~label:(bounded_rv_to_string program `Lower appr) ~outdir:output_dir ~file:input_filename program;
                  GraphPrint.print_rvg `Upper ~label:(bounded_rv_to_string program `Upper appr) ~outdir:output_dir ~file:input_filename program;
                )
              )
       )
  |> ignore
