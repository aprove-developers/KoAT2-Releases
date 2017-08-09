open Batteries
open OUnit2
open PolyTypes
open ConstraintTypes

module PolynomialParserTest =
  struct
    module Reader = Readers.Make(Mocks.TransitionGraph)

    let to_polynomial_and_back str =
         str
      |> Reader.read_polynomial
      |> Mocks.TransitionGraph.Transition_.Constraint_.Atom_.Polynomial_.to_string

    let tests =
      "Parser" >::: [
          "Positive Tests" >::: (
            List.map (fun (testname, expected, expression) ->
                testname >:: (fun _ -> assert_equal expected (to_polynomial_and_back expression)))
                     [
                       ("Constant", "42", " 42 ");
                       ("Negated Constant", "(-42)", " - 42 ");
                       ("Variable", "x", " x ");
                       ("Negated Variable", "(-x)", " - x ");
                       ("Parentheses", "x", " ( x ) ");
                       ("Double Parentheses", "x", " ( ( x ) )");
                       ("Constant Addition", "(42+24)", " 42 + 24 ");
                       ("Constant Subtraction", "(42+(-24))", " 42 - 24 ");
                       ("Constant Multiplication", "(42*24)", " 42 * 24 ");
                       ("Addition", "((x+24)+y)", " x + 24 + y ");
                       ("Subtraction", "((42+(-24))+(-x))", " 42 - 24 - x ");
                       ("Multiplication", "((42*x)*24)", " 42 * x * 24 ");
                       ("Power", "(x^3)", " x ^ 3 ");
                       ("Double Negation", "(-(-42))", " - - 42 ");
                       ("Multiplication before Addition", "(x+(y*z))", " x + y * z ");
                       ("Power before Negation", "(-(x^3))", " - x ^ 3 ");
                       ("Power before Multiplication", "((x^3)*(y^4))", " x ^ 3 * y ^ 4 ");
                     ]
          );
          "Negative Tests" >::: (
            List.map (fun (testname, expression) ->
                testname >:: (fun _ -> assert_raises Reader.Parser.Error (fun _ -> to_polynomial_and_back expression)))
                     [
                       ("Power with negative exponent", " x ^ - 3 ");
                       ("Power with variable exponent", " x ^ y ");
                       ("Power with term exponent", " x ^ (y + 3) ");
                     ]
          );
        ]

  end
  
module PolynomialTest(P : Polynomial) =
  struct
    module Reader = Readers.Make(TransitionGraph.MakeTransitionGraph(TransitionGraph.MakeTransition(Constraints.Make(P))))
               
    let example_valuation = P.Valuation_.from [(P.Var.of_string "x", P.Value.of_int 3);
                                               (P.Var.of_string "y", P.Value.of_int 5);
                                               (P.Var.of_string "z", P.Value.of_int 7)]
                                            
    let evaluate str =
         str
      |> Reader.read_polynomial
      |> fun poly -> P.eval poly example_valuation

    let assert_equal_value =
      assert_equal ~cmp:P.Value.equal ~printer:P.Value.to_string
    
    let assert_equal_string =
      assert_equal ~cmp:String.equal

    let assert_true = assert_bool ""
    let assert_false b = assert_true (not b)

    let tests =
      "Polynomial" >::: [
          "Evaluate" >::: (
            List.map (fun (testname, expected, expression) ->
                testname >:: (fun _ -> assert_equal_value (P.Value.of_int expected) (evaluate expression)))
                     [
                       ("Constant", 42, " 42 ");
                       ("Negated Constant", -42, " - 42 ");
                       ("Variable", 3, " x ");
                       ("Negated Variable", -3, " - x ");
                       ("Parentheses", 3, " ( x ) ");
                       ("Double Parentheses", 3, " ( ( x ) )");
                       ("Constant Addition", 42+24, " 42 + 24 ");
                       ("Constant Subtraction", 42-24, " 42 - 24 ");
                       ("Constant Multiplication", 42*24, " 42 * 24 ");
                       ("Addition", 32, " x + 24 + y ");
                       ("Subtraction", 15, " 42 - 24 - x ");
                       ("Multiplication", 3*42*24, " 42 * x * 24 ");
                       ("Power", 27, " x ^ 3 ");
                       ("Double Negation", 42, " - - 42 ");
                       ("Multiplication before Addition", 38, " x + y * z ");
                       ("Power before Negation", -27, " - x ^ 3 ");
                       ("Power before Multiplication", 27*625, " x ^ 3 * y ^ 4 ");
                     ];
          );
            "Math" >::: ([
                "zero" >:: (fun _ -> assert_equal_value (P.Value.of_int 0) (P.eval P.zero example_valuation));
                "one" >:: (fun _ -> assert_equal_value (P.Value.of_int 1) (P.eval P.one example_valuation));
                "constant" >::: (
                  List.map (fun (expected, expression) ->
                      expression >:: (fun _ -> assert_equal_value (P.Value.of_int expected) (P.constant (Reader.read_polynomial expression))))
                           [
                             (5, " 5 ");
                             (0, " x ");
                             (15, " 5 + 2 + x + 8 ");
                             (10, " 5 * 2 + x ");
                             (0, " 5 * 2 * x ");
                             (1, " x ^ 0 ");
                             (15, " 5 * ( x + 3 ) ");
                           ];
                );
                "is_var" >::: (
                  List.map (fun (expected, expression) ->
                      expression >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (P.is_var (Reader.read_polynomial expression))))
                           [
                             (false, " 1 ");
                             (true, " x ");
                             (true, " 1 * x ");
                             (true, " - - x ");
                             (false, " x ^ 0 ");
                             (true, " x ^ 1 ");
                             (false, " x ^ 2 ");
                             (false, " 2 * x "); 
                             (true, " 2 * x - 1 * x ");
                             (false, "6 * x ^5 *y - 2*z");
                             (true , "x*y*z*a*b*c + d - x*y*z*a*b*c");
                           ];
                );
                
                "is_univariate_linear" >::: (
                    List.map (fun (expected, expression) ->
                        expression >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (P.is_univariate_linear (Reader.read_polynomial expression))))
                            [
                                (true, " 1 ");
                                (true, " x ");
                                (false, " x^2 + y^3 ");
                                (true, " - - x ");
                                (true, " x ^ 0 ");
                                (false, " x - 100 * y + 3 * z ");
                                (false, " x ^ 2 ");
                                (true, " 2 * x "); 
                                (true, " 2 * x - 1 * x ");
                                (false, "6 * x ^5 *y - 2*z");
                                (true , "x*y*z*a*b*c + d - x*y*z*a*b*c");
                            ];
                    );
                    
                "is_linear" >::: (
                    List.map (fun (expected, expression) ->
                        expression >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (P.is_linear (Reader.read_polynomial expression))))
                            [
                                (true, " 1 ");
                                (true, " x ");
                                (false, " x^2 + y^3 ");
                                (true, " - - x ");
                                (true, " x ^ 0 ");
                                (true, " x - 100 * y + 3 * z ");
                                (false, " x ^ 2 ");
                                (true, " 2 * x "); 
                                (true, " 2 * x - 1 * x ");
                                (false, "6 * x ^5 *y - 2*z");
                                (true , "x*y*z*a*b*c + d - x*y*z*a*b*c");
                                (false, "-10*z^3 + 10 * z^3 + x*x*x*x^0");
                                (true, "x^0 + y^0 + z + f + g");
                                (true, "3");
                                (true, "0");
                            ];
                    );
              ]
            );
            

              
        ]

  end
