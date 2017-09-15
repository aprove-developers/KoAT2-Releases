open Batteries
open OUnit2
   
module StringIDAtomTest = AtomTest.Methods(Constraints.Make(Polynomials.Make(ID.StringID)(PolyTypes.OurInt)))
module StringIDConstraintsTest = ConstraintsTest.Methods(Constraints.Make(Polynomials.Make(ID.StringID)(PolyTypes.OurInt)))

module StringIDPolynomial = PolynomialsTest.Methods(PolyImpl.Polynomial)
module StringIDTemplatePolynomial = PolynomialsTest.Methods(PolyImpl.TemplatePolynomial)

                          
let suite =
  "Suite" >::: [
      "Polynomial" >::: [
        PolynomialsTest.Parser.tests;
        StringIDPolynomial.tests;
      ];
      "TemplatePolynomial" >::: [
        StringIDTemplatePolynomial.tests;

      ];
      "PolynomialConstraints" >::: [
          ConstraintsTest.Parser.tests;
          StringIDAtomTest.tests;
          StringIDConstraintsTest.tests;
        ];
      TransitionGraphTest.suite;
      SMTTest.suite;
      IDTest.tests;
    ]
                     
let () =
  run_test_tt_main suite
  
