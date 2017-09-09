open Batteries
open OUnit2
open PolyTypes
open ConstraintTypes
open Helper
   
module Parser =
  struct
    module Reader = Readers.Make(TransitionGraphImpl.StdProgram)

    let assert_equal_polynomial =
      assert_equal
        ~cmp:TransitionGraphImpl.StdProgram.Constraint_.Atom_.Polynomial_.(=~=)
        ~printer:TransitionGraphImpl.StdProgram.Constraint_.Atom_.Polynomial_.to_string

    let tests =
      "Parser" >::: [
          "Positive Tests" >::: (
            let open TransitionGraphImpl.StdProgram.Constraint_.Atom_.Polynomial_ in
            List.map (fun (testname, expected, expression) ->
                testname >:: (fun _ -> assert_equal_polynomial expected (Reader.read_polynomial expression)))
                     [
                       ("Const", value 42, " 42 ");
                       ("Negated Constant", -(value 42), " - 42 ");
                       ("Variable", var "x", " x ");
                       ("Negated Variable", -(var "x"), " - x ");
                       ("Parentheses", var "x", " ( x ) ");
                       ("Double Parentheses", var "x", " ( ( x ) )");
                       ("Constant Addition", (value 42) + (value 24), " 42 + 24 ");
                       ("Constant Subtraction", (value 42) - (value 24), " 42 - 24 ");
                       ("Constant Multiplication", (value 42) * (value 24), " 42 * 24 ");
                       ("Addition", (var "x") + (value 24) + (var "y"), " x + 24 + y ");
                       ("Subtraction", (value 42) - (value 24) - (var "x"), " 42 - 24 - x ");
                       ("Multiplication", (value 42) * (var "x") * (value 24), " 42 * x * 24 ");
                       ("Power", (var "x") ** 3, " x ^ 3 ");
                       ("Double Negation", -(-(value 42)), " - - 42 ");
                       ("Multiplication before Addition", (var "x") + (var "y") * (var "z"), " x + y * z ");
                       ("Power before Negation", - (var "x") ** 3, " - x ^ 3 ");
                       ("Power before Multiplication", (var "x") ** 3 * (var "y") ** 4, " x ^ 3 * y ^ 4 ");
                     ]
          );
          "Negative Tests" >::: (
            List.map (fun (testname, expression) ->
                testname >:: (fun _ -> assert_exception (fun _ -> Reader.read_polynomial expression)))
                     [
                       ("Power with negative exponent", " x ^ - 3 ");
                       ("Power with variable exponent", " x ^ y ");
                       ("Power with term exponent", " x ^ (y + 3) ");
                     ]
          );
        ]

  end
  
module Methods (P : Polynomial) =
  struct
    module Reader = Readers.Make(TransitionGraph.MakeProgram(Constraints.Make(P)))
               
    let example_valuation = P.Valuation_.from_native [("x", 3); ("y", 5); ("z", 7)]
                                            
    let evaluate str =
         str
      |> Reader.read_polynomial
      |> fun poly -> P.eval poly example_valuation

    let assert_equal_value =
      assert_equal ~cmp:P.Value.equal ~printer:P.Value.to_string

    let assert_equal_polynomial =
      assert_equal ~cmp:P.(=~=) ~printer:P.to_string
      
    let of_int = P.Value.of_int

    let of_string = P.Var.of_string
    
    let rec list_equality (xs : P.Value.t list ) (ys : P.Value.t list) =
        match (xs, ys) with
            |([],[]) -> true
            |(x::tailxs, y::tailys) ->  ( P.Value.Compare.(=) x y ) && (list_equality tailxs tailys)
            | (_,_) -> false
            
    let rec list_print (xs : P.Value.t list ) = String.concat "," (List.map P.Value.to_string xs)
    
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
                     
          "from_coeff_list" >::: (
            List.map (fun (expected, coeffs, vars) ->
                expected >:: (fun _ -> assert_equal~cmp:P.(=~=) ~printer:P.to_string (P.from_coeff_list coeffs vars) (Reader.read_polynomial expected)))
                     [
                       ("2 * x + 3 * y - 4 * z", [(of_int 2); (of_int 3); (of_int (-4))],[(of_string "x"); (of_string "y"); (of_string "z")]);
                       ("0", [(of_int 2); (of_int 3)],[(of_string "x"); (of_string "y"); (of_string "z")]);
                       ("0", [(of_int 2); (of_int 3); (of_int (-4))],[(of_string "z")]);
                       ("(-1) * a + 3 * b - 2 * c", [(of_int (-1)); (of_int 3); (of_int (-2))],[(of_string "a"); (of_string "b"); (of_string "c")]);
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
                      expression >:: (fun _ -> assert_equal_bool expected (P.is_var (Reader.read_polynomial expression))))
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
                        expression >:: (fun _ -> assert_equal_bool expected (P.is_univariate_linear (Reader.read_polynomial expression))))
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
                        expression >:: (fun _ -> assert_equal_bool expected (P.is_linear (Reader.read_polynomial expression))))
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

                "scale_coefficients" >::: (
                    List.map (fun (expression, expected) ->
                        "unnamed" >:: (fun _ ->
                                let poly_of_coefficients (x, y) =
                                  P.((value x) * (var "x") + (value y) * (var "y")) in
                                assert_equal_polynomial (poly_of_coefficients expected) (P.scale_coefficients (poly_of_coefficients expression))))
                             (
                               let tests = [
                                   ((1, 0), (1, 0));
                                   ((2, 0), (1, 0));
                                   ((3, 0), (1, 0));
                                   ((4, 2), (2, 1));
                                   ((8, 2), (4, 1));
                                   ((12, 8), (3, 2));
                                   ((7, 5), (7, 5));
                                   ((3, 3), (1, 1));
                                 ] in
                               List.append
                                 (* Positive coefficients *) tests
                                 (* Negative coefficients *) (List.map (fun ((x, y), (a, b)) -> ((-x, -y), (-a, -b))) tests)
                             )
                    );

                "substitute" >::: (
                    List.map (fun (expected, substitution, polynomial) ->
                        polynomial >:: (fun _ -> assert_equal_polynomial
                                                              (Reader.read_polynomial expected)
                                                              (P.substitute (P.Var.of_string "x")
                                                                            ~replacement:(Reader.read_polynomial substitution)
                                                                            (Reader.read_polynomial polynomial))))
                             [
                               (* No variables -> No change *)
                               (" 0 ", " 1 ", " 0 ");
                               (" 0 ", " x ", " 0 ");
                               (* Variable does not occur -> No change *)
                               (" y ", " 1 ", " y ");
                               (" y ", " x ", " y ");
                               (* Polynomial is just variable *)
                               (" 1 ", " 1 ", " x ");
                               (" y ", " y ", " x ");
                               (" 2 * x ", " 2 * x ", " x ");
                               (* More complex *)
                               (" 2 ", " 1 ", " 2 * x ");
                               (" 2 * x ", " x ", " 2 * x ");
                               (" 4 * x ", " 2 * x ", " 2 * x ");
                               (" 1 ", " 1 ", " x ^ 2 ");
                               (" x ^ 2 ", " x ", " x ^ 2 ");
                               (" x ^ 4 ", " x ^ 2 ", " x ^ 2 ");
                               (" 4 * x ^ 2  ", " 2 * x ", " x ^ 2 ");
                               (* Variable occurs multiple times *)
                               (" 2 ", " 1 ", " x + x ");
                               (" 2 ", " 1 ", " x * x + x ^ 2 ");
                               (" y * y + y ^ 2 ", " y ", " x * x + x ^ 2 ");
                            ];
                    );

                          ]
            );
            

              
        ]

  end
