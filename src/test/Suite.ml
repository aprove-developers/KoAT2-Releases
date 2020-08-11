open Batteries
open OUnit2

let suite =
  "Suite" >::: [
      VarSetTest.tests;
      "Polynomial" >::: [
        PolynomialsTest.Parser.tests;
        PolynomialsTest.Methods.tests;
      ];
      "PolynomialConstraints" >::: [
          ConstraintsTest.Parser.tests;
          AtomTest.Methods.tests;
          ConstraintsTest.Methods.tests;
        ];
      TransitionGraphTest.tests;
      SMTTest.suite;
      IDTest.tests;
      LocalSizeBoundTest.tests;
      FormulaTest.tests;
      PreprocessorTest.tests;
      BoundTest.tests;
      TimeboundTest.tests;
      InvariantGenerationTest.tests;

      (* Probabilistic *)
      LexRSMTest.tests;
      ELSBTest.tests;
      ExpTrivSizeBoundTest.tests;
      ExpTimeBoundTest.tests;
      ExpSizeComplTest.tests;
    ]

let () =
  Printf.printf "Some tests may take a rather long time (1-5 min)\n";
  run_test_tt_main suite
