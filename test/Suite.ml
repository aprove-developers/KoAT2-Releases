open Batteries
open OUnit2
   
module StringIDAtomTest = AtomTest.Methods(Constraints.Make(Polynomials.Make(ID.StringID)(Number.MakeNumeric(Big_int))))
module StringIDConstraintsTest = ConstraintsTest.Methods(Constraints.Make(Polynomials.Make(ID.StringID)(Number.MakeNumeric(Big_int))))

module StringIDPolynomial = PolynomialsTest.Methods(PolyImpl.Polynomial)
                          
let suite =
  "Suite" >::: [
      "Polynomial" >::: [
        PolynomialsTest.Parser.tests;
        StringIDPolynomial.tests;
      ];
      "PolynomialConstraints" >::: [
          ConstraintsTest.Parser.tests;
          StringIDAtomTest.tests;
          AtomTest.Parser.tests;
          StringIDConstraintsTest.tests;
        ];
      TransitionGraphTest.suite;
      SMTTest.suite;
    ]
                     
let () =
  run_test_tt_main suite
