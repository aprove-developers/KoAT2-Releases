open OUnit2
open Koat2
open! OurBase
open Bounds
open ProbabilisticProgramModules
open Approximation.Probabilistic

let classical_local_conf : (NonProbOverappr.program_modules_t, Bounds.Bound.t) Analysis.local_configuration =
  Analysis.default_local_configuration


let default_conf = ProbabilisticAnalysis.default_configuration

let mprf5_conf =
  {
    ProbabilisticAnalysis.default_configuration with
    classical_local = { classical_local_conf with run_mprf_depth = Some 5 };
  }


let twn_conf =
  {
    ProbabilisticAnalysis.default_configuration with
    classical_local = { classical_local_conf with twn = true };
  }


let mprf5_twn_conf =
  {
    ProbabilisticAnalysis.default_configuration with
    classical_local = { classical_local_conf with run_mprf_depth = Some 5; twn = true };
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
                ~f:(fun (name, complexity_exp, prog_dir, conf) ->
                  (fun f -> name >: test_case ~length:test_len f) @@ fun _ ->
                  let prog = Readers.read_probabilistic_program (prog_dir ^ name ^ ".koat") |> preprocess in
                  let prog, apprs = ProbabilisticAnalysis.perform_analysis ~conf prog in

                  let bound = ExpApproximation.program_timebound apprs.appr prog in
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
                    ("leading_tacas21", Polynomial 2, "../../../examples/probabilistic/", default_conf);
                    ("leading_tacas21.1", Polynomial 2, "../../../examples/probabilistic/", default_conf);
                    ("simple_multdist", Polynomial 1, "../../../examples/probabilistic/", default_conf);
                    ( "simple_multdist_advanced",
                      Polynomial 1,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "probabilistic_nested_bounded",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ("probabilistic_nested_unbounded", Inf, "../../../examples/probabilistic/", mprf5_twn_conf);
                    ( "nested_mprf_inner_loop_exptime_classsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "nested_mprf_inner_loop_exptime_classsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      mprf5_conf );
                    ( "nested_mprf_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "nested_mprf_inner_loop_classtime_expsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      mprf5_conf );
                    ( "nested_twn_inner_loop_exptime_classsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "nested_twn_inner_loop_exptime_classsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      twn_conf );
                    ( "nested_twn_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
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
                    ( "prob_loop",
                      Polynomial 1,
                      "../../../examples/probabilistic/tacas21_paper/koat/",
                      default_conf );
                    ( "simple_nested",
                      Polynomial 2,
                      "../../../examples/probabilistic/tacas21_paper/koat/",
                      default_conf );
                    ("non_past_rwalk", Inf, "../../../examples/probabilistic/", mprf5_twn_conf);
                    ("non_past_size", Inf, "../../../examples/probabilistic/", mprf5_twn_conf);
                  ];
       ]
