open Batteries
open Parameter
open ProofOutput
open Goal

let run probabilistic_goal (params: params) =
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
  );

  let result_print goal =
    match goal with
    | Goal.ExpectedComplexity -> (
      match params.result with
      |"termcomp" -> print_termcomp_expected
      |"all" -> print_all_expected_bounds ~html:params.html
      |_ -> print_overall_expected_costbound ~html:params.html
    )

    | Goal.ExpectedSize v -> (
      match params.result with
      |"termcomp" -> print_termcomp_expected_size v
      |"all" -> print_all_expected_bounds_expected_size v ~html:params.html
      |_ -> print_overall_expected_sizebound ~html:params.html v
    )
  in

  let cache = CacheManager.new_cache () in

  let program_and_goal =
    input
    |> MainUtil.read_input_varlist (CacheManager.trans_id_counter cache) ~rename:params.rename
    (* Print system before renaming *)
    |> tap (Option.may (fun (program,_) ->
        if params.print_system_for_paper then
          GraphPrint.print_system_for_paper ~format:params.print_system_for_paper_format ~outdir:output_dir ~file:input_filename program))
    (* Rename program and the target variable of goal EXPECTEDSIZE *)
    |> flip Option.Monad.bind (
        fun (p, vs) -> match probabilistic_goal with
          | ExpectedSize v -> (
            let renamed_program, arg_vars = rename_program p in
            let i = List.index_of v vs in
            match i with
            | Some i ->
                Some (renamed_program, ExpectedSize (Var.mk_arg i))
            | None ->
                prerr_string @@
                  "The variable " ^ Var.to_string v
                  ^ " in goal "^Goal.to_string (ProbabilisticGoal probabilistic_goal)
                  ^ " does not exist. Possible values include " ^ Util.enum_to_string Var.to_string (List.enum vs);
                None
          )
          | g -> Some (Tuple2.first @@ rename_program p, g)
        )
  in
  program_and_goal
  |> tap (Option.may (fun (program, goal) ->
      if params.print_system_for_paper then
        GraphPrint.print_system_for_paper ~format:params.print_system_for_paper_format ~outdir:output_dir ~file:input_filename program))
  |> Option.map (fun (program, goal) ->
         (program, Approximation.create program)
         |> tap (fun (program, appr) ->
                if params.print_system_id then
                  GraphPrint.print_system ~label:TransitionLabel.to_id_string ~outdir:output_dir ~file:(input_filename ^ "_id") program)
         |> Preprocessor.process (CacheManager.trans_id_counter cache) params.preprocessing_strategy params.preprocessors
         |> tap (fun (program, appr) ->
                if params.print_system then
                  GraphPrint.print_system ~label:TransitionLabel.to_string ~outdir:output_dir ~file:input_filename program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  GraphPrint.print_rvg (CacheManager.lsb_cache cache) `Lower ~label:RV.to_id_string ~outdir:output_dir ~file:input_filename program;
                  GraphPrint.print_rvg (CacheManager.lsb_cache cache) `Upper ~label:RV.to_id_string ~outdir:output_dir ~file:input_filename program
                )
              )
         |> (fun (program, appr) ->
                   if not params.no_boundsearch then
                     (program, appr)
                     |> uncurry
                        (Bounds.find_exp_bounds
                          params.simplify_prob_smt
                          ~refined_smt_timeout:(Some (Float.of_int params.refined_smt_timeout))
                          ~generate_invariants_bottom_up:Preprocessor.generate_invariants params.bottom_up cache
                        )
                   else (program, appr))
         |> tap (fun (program, appr) -> result_print goal program appr)
         |> tap (fun (program, appr) ->
                if params.print_system then
                  GraphPrint.print_system ~label:(bounded_label_to_string appr) ~outdir:output_dir ~file:(input_filename ^ "_bounded" ) program)
         |> tap (fun (program, appr) ->
                if params.print_rvg then (
                  GraphPrint.print_rvg (CacheManager.lsb_cache cache) `Lower ~label:(bounded_rv_to_string (CacheManager.lsb_cache cache) program `Lower appr) ~outdir:output_dir ~file:input_filename program;
                  GraphPrint.print_rvg (CacheManager.lsb_cache cache) `Upper ~label:(bounded_rv_to_string (CacheManager.lsb_cache cache) program `Upper appr) ~outdir:output_dir ~file:input_filename program;
                )
              )
         |> tap (fun (program, appr) ->
                if params.print_ervg then (
                  GraphPrint.print_ervg (CacheManager.elsb_cache cache) ~label:(bounded_erv_to_string (CacheManager.elsb_cache cache) program appr) ~outdir:output_dir ~file:input_filename program;
                )
              )
       )
  |> ignore
