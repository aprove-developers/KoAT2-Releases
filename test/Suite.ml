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
      TransitionGraphTest.suite;
      SMTTest.suite;
      IDTest.tests;
      LocalSizeBoundTest.tests;
      FormulaTest.tests;
      PreprocessorTest.tests;
      BoundTest.tests;
      TimeboundTest.tests;
      InvariantGenerationTest.tests;
      OurRationalTest.tests;
      TWNTest.tests;
      SolvableTest.tests;

      ProbabilityDistributionTests.tests;
      UpdateElementTests.tests;
      ExpectedLocalChangeBoundTests.tests;
    ]

let () =
  run_test_tt_main suite

