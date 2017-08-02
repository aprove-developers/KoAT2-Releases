open Batteries
open OUnit2
   
module StringIDPolynomialConstraintsAtomTest = PolynomialConstraintsTest.PolynomialConstraintsAtomTest(Constraints.Make(Polynomials.Make(ID.StringID)(Number.MakeNumeric(Big_int))))
module StringIDPolynomialConstraintsTest = PolynomialConstraintsTest.PolynomialConstraintsTest(Constraints.Make(Polynomials.Make(ID.StringID) (Number.MakeNumeric(Big_int))))

module StringIDPolynomial = PolynomialsTest.PolynomialTest(StdPoly.Polynomial)
                          
let suite =
  "Suite" >::: [
      "Polynomial" >::: [
        PolynomialsTest.PolynomialParserTest.tests;
        StringIDPolynomial.tests;
      ];
      "PolynomialConstraints" >::: [
          PolynomialConstraintsTest.PolynomialConstraintsParserTest.tests;
          StringIDPolynomialConstraintsAtomTest.tests;
          PolynomialConstraintsTest.PolynomialConstraintsAtomParserTest.tests;
          StringIDPolynomialConstraintsTest.tests
        ];
      TransitionGraphTest.suite;
      SMTTest.suite;
    ]
                     
let () =
  run_test_tt_main suite
