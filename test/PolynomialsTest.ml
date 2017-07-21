open Batteries
open ID
open Z3
open OUnit2
open PolyTypes
   
module PolynomialTest(Var : ID) =
  struct
               
    module AST = PolynomialAST(Var)
    module Parser = PolynomialParser.Make(Var)
    module Lexer = PolynomialLexer.Make(Var)
    module Polynomial = Polynomials.MakePolynomial(Var)(Number.MakeNumeric(Big_int))
    module Valuation = Valuation.MakeValuation(Var)(Number.MakeNumeric(Big_int))

    (* Unambigous transformation to string to make tests more readable *)
    let rec polynomial_to_string (ex : AST.t) = match ex with
      | AST.Constant c -> string_of_int c
      | AST.Variable v -> Var.to_string v
      | AST.Neg t -> String.concat "" ["("; "-"; polynomial_to_string t; ")"]
      | AST.Plus (t1,t2) -> String.concat "" ["("; polynomial_to_string t1; "+"; polynomial_to_string t2; ")"]
      | AST.Times (t1,t2) -> String.concat "" ["("; polynomial_to_string t1; "*"; polynomial_to_string t2; ")"]
      | AST.Pow (v,n) -> String.concat "" ["("; Var.to_string v; "^"; string_of_int n; ")"]
                       
    let to_ast str =
         str
      |> Lexing.from_string
      |> Parser.polynomial Lexer.read

    let to_polynomial str =
         str
      |> to_ast
      |> Polynomial.from_ast

       
    let to_ast_and_back str =
         str
      |> to_ast
      |> polynomial_to_string

    let example_valuation = Valuation.from [(Var.of_string "x", Big_int.of_int 3);
                                            (Var.of_string "y", Big_int.of_int 5);
                                            (Var.of_string "z", Big_int.of_int 7)]

    let evaluate str =
         str
      |> to_ast
      |> Polynomial.from_ast
      |> fun poly -> Polynomial.eval poly example_valuation

    let assert_equal_big_int =
      assert_equal ~cmp:Big_int.equal ~printer:Big_int.to_string
    
    let assert_equal_string =
      assert_equal ~cmp:String.equal

    let assert_true = assert_bool ""
    let assert_false b = assert_true (not b)

    let parser_tests =
      "Parser" >::: [
          "Positive Tests" >::: (
            List.map (fun (testname, expected, expression) ->
                testname >:: (fun _ -> assert_equal expected (to_ast_and_back expression)))
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
                testname >:: (fun _ -> assert_raises Parser.Error (fun _ -> to_ast_and_back expression)))
                     [
                       ("Power with negative exponent", " x ^ - 3 ");
                       ("Power with variable exponent", " x ^ y ");
                       ("Power with term exponent", " x ^ (y + 3) ");
                     ]
          );
        ]

    let poly_tests =
      "Polynomial" >::: [
          "Evaluate" >::: (
            List.map (fun (testname, expected, expression) ->
                testname >:: (fun _ -> assert_equal_big_int (Big_int.of_int expected) (evaluate expression)))
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
                "zero" >:: (fun _ -> assert_equal_big_int (Big_int.of_int 0) (Polynomial.eval Polynomial.zero example_valuation));
                "one" >:: (fun _ -> assert_equal_big_int (Big_int.of_int 1) (Polynomial.eval Polynomial.one example_valuation));
                "constant" >::: (
                  List.map (fun (expected, expression) ->
                      expression >:: (fun _ -> assert_equal_big_int (Big_int.of_int expected) (Polynomial.constant (to_polynomial expression))))
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
                      expression >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (Polynomial.is_var (to_polynomial expression))))
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
                        expression >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (Polynomial.is_univariate_linear (to_polynomial expression))))
                            [
                                (false, " 1 ");
                                (true, " x ");
                                (false, " x^2 + y^3 ");
                                (true, " - - x ");
                                (false, " x ^ 0 ");
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
                        expression >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (Polynomial.is_linear (to_polynomial expression))))
                            [
                                (false, " 1 ");
                                (true, " x ");
                                (false, " x^2 + y^3 ");
                                (true, " - - x ");
                                (false, " x ^ 0 ");
                                (true, " x - 100 * y + 3 * z ");
                                (false, " x ^ 2 ");
                                (true, " 2 * x "); 
                                (true, " 2 * x - 1 * x ");
                                (false, "6 * x ^5 *y - 2*z");
                                (true , "x*y*z*a*b*c + d - x*y*z*a*b*c");
                                (false, "-10*z^3 + 10 * z^3 + x*x*x*x^0");
                                (true, "x^0 + y^0 + z + f + g");
                            ];
                    );
              ]
            );
            

              
        ]

  end

module StringIDPolynomial = PolynomialTest(StringID)

let suite =
  "Suite" >::: [
      StringIDPolynomial.parser_tests;
      StringIDPolynomial.poly_tests;
    ]
                     
let () =
  run_test_tt_main suite
