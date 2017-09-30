open Batteries
open OUnit2
                             
let suite =
  "Suite" >::: [
      "Polynomial" >::: [
        PolynomialsTest.Parser.tests;
        PolynomialsTest.Methods.tests;
      ];
      "TemplatePolynomial" >::: [
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
    ]
                     
let () =
  run_test_tt_main suite
  
