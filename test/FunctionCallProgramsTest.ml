open OUnit2
open Koat2
open! OurBase
open Bounds

let twn_conf =
  Analysis.
    {
      default_configuration with
      local_configuration = { default_local_configuration with twn = true; twnlog = true };
    }


module Analysis = Analysis.Classical (Bounds.Bound)

let preprocess =
  Preprocessor.StandardProgram.process Preprocessor.process_till_fixpoint Preprocessor.default_classical


let tests =
  "FCTimeBoundsTests"
  >::: [
         "asymptotic"
         >::: List.map
                ~f:(fun (name, complexity_exp, prog_dir, conf) ->
                  prog_dir >:: fun _ ->
                  let prog = Readers.read_file (prog_dir ^ name ^ ".koat") |> preprocess in
                  let prog, appr = Analysis.improve ~conf ~preprocess prog Approximation.empty in

                  let bound = Approximation.program_timebound appr prog in
                  let complexity = Bound.asymptotic_complexity @@ bound in

                  let error_msg =
                    "FCTimeBoundsTests(" ^ name ^ "): Asymptotic Complexity "
                    ^ Bound.show_complexity complexity ^ " of bound " ^ Bound.to_string bound
                    ^ " does not match expected complexity " ^ Bound.show_complexity complexity_exp
                  in

                  assert_bool error_msg (Bound.equal_complexity complexity complexity_exp))
                Bound.
                  [
                    ( "fc01",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc02",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc03",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc04",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ("fc05", Exponential 1, "../../../examples/rec/paper/", twn_conf);
                    ( "fc06",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc07",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 2),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ("fc08", Exponential 1, "../../../examples/rec/paper/", twn_conf);
                    ("fc09", Exponential 1, "../../../examples/rec/paper/", twn_conf);
                    ( "fc10",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc11",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc12",
                      LogarithmicPolynomial (OurRational.of_int 1, OurRational.of_int 1),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc13",
                      LogarithmicPolynomial (OurRational.of_int 0, OurRational.of_int 2),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ( "fc14",
                      LogarithmicPolynomial (OurRational.of_int 1, OurRational.of_int 2),
                      "../../../examples/rec/paper/",
                      twn_conf );
                    ("fc15", Inf, "../../../examples/rec/paper/", twn_conf);
                  ];
       ]
