open OUnit2
open Koat2
open! OurBase
open Bounds
open ProbabilisticProgramModules
open Approximation.Probabilistic

let classic_conf : NonProbOverappr.program_modules_t Analysis.analysis_configuration =
  Analysis.default_configuration


let mprf5_conf : NonProbOverappr.program_modules_t Analysis.analysis_configuration =
  { classic_conf with run_mprf_depth = Some 5 }


let twn_conf : NonProbOverappr.program_modules_t Analysis.analysis_configuration =
  { classic_conf with twn_configuration = Some `NoTransformation }


let mprf5_twn_conf : NonProbOverappr.program_modules_t Analysis.analysis_configuration =
  {
    classic_conf with
    run_mprf_depth = mprf5_conf.run_mprf_depth;
    twn_configuration = twn_conf.twn_configuration;
  }


let conf = ProbabilisticAnalysis.default_configuration

let preprocess =
  Preprocessor.ProbabilisticWithOverappr.process Preprocessor.process_till_fixpoint
    Preprocessor.all_probabilistic


let test_len = OUnitTest.Custom_length 5.

let tests =
  "ExpTimeBoundsTests"
  >::: [
         "asymptotic"
         >::: List.map
                ~f:(fun (name, complexity_exp, prog_dir, classic_conf) ->
                  (fun f -> name >: test_case ~length:test_len f) @@ fun _ ->
                  let prog = Readers.read_probabilistic_program (prog_dir ^ name ^ ".koat") |> preprocess in
                  let prog, (_, appr) =
                    ProbabilisticAnalysis.perform_classic_and_probabilistic_analysis ~classic_conf ~conf prog
                  in

                  let bound = ExpApproximation.program_timebound appr prog in
                  let complexity = RationalBound.asymptotic_complexity bound in

                  let error_msg =
                    "Asymptotic Complexity "
                    ^ RationalBound.show_complexity complexity
                    ^ " of bound " ^ RationalBound.to_string bound ^ " does not match expected complexity "
                    ^ RationalBound.show_complexity complexity_exp
                  in

                  assert_bool error_msg (RationalBound.equal_complexity complexity complexity_exp))
                RationalBound.
                  [
                    ("leading_tacas21", Polynomial 2, "../../../examples/probabilistic/", classic_conf);
                    ("leading_tacas21.1", Polynomial 2, "../../../examples/probabilistic/", classic_conf);
                    ("simple_multdist", Polynomial 1, "../../../examples/probabilistic/", classic_conf);
                    ( "simple_multdist_advanced",
                      Polynomial 1,
                      "../../../examples/probabilistic/",
                      classic_conf );
                    ( "probabilistic_nested_bounded",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      classic_conf );
                    ("probabilistic_nested_unbounded", Inf, "../../../examples/probabilistic/", mprf5_twn_conf);
                    ( "nested_mprf_inner_loop_exptime_classsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      classic_conf );
                    ( "nested_mprf_inner_loop_exptime_classsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      mprf5_conf );
                    ( "nested_mprf_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      classic_conf );
                    ( "nested_mprf_inner_loop_classtime_expsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      mprf5_conf );
                    ( "nested_twn_inner_loop_exptime_classsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      classic_conf );
                    ( "nested_twn_inner_loop_exptime_classsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      twn_conf );
                    ( "nested_twn_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      classic_conf );
                    ( "nested_twn_inner_loop_classtime_expsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      twn_conf );
                    ( "nested_twn003_inner_loop_exptime_classsize",
                      Polynomial 3,
                      "../../../examples/probabilistic/",
                      twn_conf );
                    ( "nested_twn003_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      twn_conf );
                    ( "nested_twn003_inner_loop_classtime_mixedsize",
                      Polynomial 3,
                      "../../../examples/probabilistic/",
                      twn_conf );
                    ( "nested_twn003_inner_loop_classtime_mixedsize_exponential",
                      Exponential 1,
                      "../../../examples/probabilistic/",
                      twn_conf );
                  ];
       ]
