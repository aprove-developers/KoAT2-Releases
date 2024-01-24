open OUnit2
open Koat2
open! OurBase
open Bounds
open Approximation.Probabilistic

let default_conf = ProbabilisticAnalysis.default_configuration

let enable_mprf5 conf =
  ProbabilisticAnalysis.{ conf with classical_local = { conf.classical_local with run_mprf_depth = Some 5 } }


let enable_twn conf =
  ProbabilisticAnalysis.{ conf with classical_local = { conf.classical_local with twn = true } }


let enable_cfr_default conf =
  ProbabilisticAnalysis.{ conf with cfrs = [ CFR.pe_native_probabilistic Abstraction.{ abstract = `FVS } ] }


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
                    ( "probabilistic_nested_unbounded",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn |> enable_mprf5 );
                    ( "nested_mprf_inner_loop_exptime_classsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "nested_mprf_inner_loop_exptime_classsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_mprf5 );
                    ( "nested_mprf_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "nested_mprf_inner_loop_classtime_expsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_mprf5 );
                    ( "nested_twn_inner_loop_exptime_classsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "nested_twn_inner_loop_exptime_classsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn );
                    ( "nested_twn_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf );
                    ( "nested_twn_inner_loop_classtime_expsize",
                      Polynomial 2,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn );
                    ( "nested_twn003_inner_loop_exptime_classsize",
                      Polynomial 3,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn );
                    ( "nested_twn003_inner_loop_classtime_expsize",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn );
                    ( "nested_twn003_inner_loop_classtime_mixedsize",
                      Polynomial 3,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn );
                    ( "nested_twn003_inner_loop_classtime_mixedsize_exponential",
                      Exponential 1,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn );
                    ( "prob_loop",
                      Polynomial 1,
                      "../../../examples/probabilistic/tacas21_paper/koat/",
                      default_conf );
                    ( "simple_nested",
                      Polynomial 2,
                      "../../../examples/probabilistic/tacas21_paper/koat/",
                      default_conf );
                    ( "non_past_rwalk",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn |> enable_mprf5 );
                    ( "non_past_size",
                      Inf,
                      "../../../examples/probabilistic/",
                      default_conf |> enable_twn |> enable_mprf5 );
                    ( "simple_control_on_input_nonprob",
                      Inf,
                      "../../../examples/probabilistic/cfr/",
                      default_conf );
                    ( "simple_control_on_input_nonprob",
                      Polynomial 0,
                      "../../../examples/probabilistic/cfr/",
                      default_conf |> enable_cfr_default );
                    ( "cfr_leading",
                      Polynomial 1,
                      "../../../examples/probabilistic/cfr/",
                      default_conf |> enable_cfr_default );
                  ];
       ]
