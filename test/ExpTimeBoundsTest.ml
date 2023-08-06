open OUnit2
open Koat2
open OurBase
open Bounds
open ProbabilisticProgramModules
open Approximation.Probabilistic

let classic_conf = Analysis.default_configuration
let conf = ProbabilisticAnalysis.default_configuration


let preprocess =
  Preprocessor.ProbabilisticWithOverappr.process
    Preprocessor.process_till_fixpoint Preprocessor.all_probabilistic

let test_len = OUnitTest.Custom_length 5.
let tests =
  "ExpTimeBoundsTests" >:::[
    "asymptotic" >:::
      List.map ~f:(fun (name, complexity_exp, prog_dir) ->
        (fun f  -> name >: test_case ~length:test_len f) @@ fun _ ->
            let prog =
              Readers.read_probabilistic_program (prog_dir ^ name ^ ".koat")
              |> preprocess
            in
            let prog, (_, appr) =
            ProbabilisticAnalysis.perform_classic_and_probabilistic_analysis
                ~classic_conf ~conf prog
            in

            let bound  = ExpApproximation.program_timebound appr prog in
            let complexity = RealBound.asymptotic_complexity bound in

            let error_msg =
            "Asymptotic Complexity " ^ RealBound.show_complexity complexity
            ^ " of bound " ^ RealBound.to_string bound
            ^ " does not match expected complexity " ^ RealBound.show_complexity complexity_exp
            in

            assert_bool error_msg (RealBound.equal_complexity complexity complexity_exp)

        )
        RealBound.[
          ("leading_tacas21"  , Polynomial 2, "../../../examples/probabilistic/")
        ; ("leading_tacas21.1", Polynomial 2, "../../../examples/probabilistic/")
        ; ("simple_multdist"  , Polynomial 1, "../../../examples/probabilistic/")
        ]
  ]
