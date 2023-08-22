open Koat2
open Batteries
open OUnit2
open Helper
open Polynomials

module Parser = struct
  let tests =
    "Parser"
    >::: [
           ("Positive Tests"
           >:::
           let open Polynomial in
           List.map
             (fun (testname, expected, expression) ->
               testname >:: fun _ -> assert_equal_poly expected (Readers.read_polynomial expression))
             [
               ("Const", value 42, " 42 ");
               ("Negated Constant", -value 42, " - 42 ");
               ("Variable", var "x", " x ");
               ("Negated Variable", -var "x", " - x ");
               ("Parentheses", var "x", " ( x ) ");
               ("Double Parentheses", var "x", " ( ( x ) )");
               ("Constant Addition", value 42 + value 24, " 42 + 24 ");
               ("Constant Subtraction", value 42 - value 24, " 42 - 24 ");
               ("Constant Multiplication", value 42 * value 24, " 42 * 24 ");
               ("Addition", var "x" + value 24 + var "y", " x + 24 + y ");
               ("Subtraction", value 42 - value 24 - var "x", " 42 - 24 - x ");
               ("Multiplication", value 42 * var "x" * value 24, " 42 * x * 24 ");
               ("Power", var "x" ** 3, " x ^ 3 ");
               ("Double Negation", - (-value 42), " - - 42 ");
               ("Multiplication before Addition", var "x" + (var "y" * var "z"), " x + y * z ");
               ("Power before Negation", -var "x" ** 3, " - x ^ 3 ");
               ("Power before Multiplication", (var "x" ** 3) * (var "y" ** 4), " x ^ 3 * y ^ 4 ");
             ]);
           "Negative Tests"
           >::: List.map
                  (fun (testname, expression) ->
                    testname >:: fun _ -> assert_exception (fun _ -> Readers.read_polynomial expression))
                  [
                    ("Power with negative exponent", " x ^ - 3 ");
                    ("Power with variable exponent", " x ^ y ");
                    ("Power with term exponent", " x ^ (y + 3) ");
                  ];
         ]
end

module Methods = struct
  module Valuation = Valuation.Make (OurInt)

  let example_valuation = Valuation.from_native [ ("x", 3); ("y", 5); ("z", 7) ]
  let evaluate str = str |> Readers.read_polynomial |> fun poly -> Polynomial.eval poly example_valuation

  let example_valuation2 =
    Valuation.from
      [
        (Var.mk_helper Var.Int 1, OurInt.of_int 1);
        (Var.mk_helper Var.Int 2, OurInt.of_int 0);
        (Var.mk_helper Var.Int 3, OurInt.of_int (-1));
      ]


  let evaluate_partial poly = Polynomial.eval_partial poly example_valuation2
  let of_int = OurInt.of_int
  let of_string = Var.of_string

  let rec list_equality (xs : OurInt.t list) (ys : OurInt.t list) =
    match (xs, ys) with
    | [], [] -> true
    | x :: tailxs, y :: tailys -> OurInt.(x =~= y) && list_equality tailxs tailys
    | _, _ -> false


  let list_print (xs : OurInt.t list) = String.concat "," (List.map OurInt.to_string xs)

  let tests =
    "Polynomial"
    >::: [
           "pull_out_common_addends"
           >::: List.map
                  (fun (t1_s, t2_s, (t'_s, (t1'_s, t2'_s))) ->
                    let t1 = Readers.read_polynomial t1_s in
                    let t2 = Readers.read_polynomial t2_s in
                    let t' = Readers.read_polynomial t'_s in
                    let t1' = Readers.read_polynomial t1'_s in
                    let t2' = Readers.read_polynomial t2'_s in

                    let t'_comp, (t1'_comp, t2'_comp) = Polynomial.pull_out_common_addends t1 t2 in
                    Polynomial.to_string t1 ^ "," ^ Polynomial.to_string t2
                    >::: [
                           ("t'" >:: fun _ -> assert_equal_poly t' t'_comp);
                           ("t1'" >:: fun _ -> assert_equal_poly t1' t1'_comp);
                           ("t2'" >:: fun _ -> assert_equal_poly t2' t2'_comp);
                         ])
                  [
                    ("X", "2*X", ("X", ("0", "X")));
                    ("2*X", "X", ("X", ("X", "0")));
                    ("2*X", "2*X", ("2*X", ("0", "0")));
                    ("X", "-2*X", ("0", ("X", "-2*X")));
                    ("-2*X", "X", ("0", ("-2*X", "X")));
                    ("-2*X", "2*X", ("0", ("-2*X", "2*X")));
                    ("-X", "-2*X", ("-X", ("0", "-1*X")));
                    ("-2*X", "-X", ("-X", ("-1*X", "0")));
                    ("-2*X", "-2*X", ("-2*X", ("0", "0")));
                    ("2*X + 3*Y", "3*X + 4*Z", ("2*X", ("3*Y", "X + 4*Z")));
                    ("3*X + 4*Z", "2*X + 3*Y", ("2*X", ("X + 4*Z", "3*Y")));
                    ("4*X + 3*X*Y", "2*X", ("2*X", ("2*X + 3*X*Y", "0")));
                    ("2*X", "4*X + 3*X*Y", ("2*X", ("0", "2*X + 3*X*Y")));
                    ("5*X*Y", "4*X + 3*X*Y", ("3*X*Y", ("2*X*Y", "4*X")));
                    ("4*X + 3*X*Y", "5*X*Y", ("3*X*Y", ("4*X", "2*X*Y")));
                    ("5*X*Y + X", "4*X + 3*X*Y", ("X + 3*X*Y", ("2*X*Y", "3*X")));
                    ("4*X + 3*X*Y", "5*X*Y + X", ("X + 3*X*Y", ("3*X", "2*X*Y")));
                  ];
           "String_matching"
           >::: List.map
                  (fun (expected, input_str) ->
                    "input_str" >:: fun _ ->
                    assert_equal ~cmp:Var.( =~= ) ~printer:Var.to_string expected (Var.of_string input_str))
                  [
                    (Var.of_string "x", "x");
                    (Var.mk_helper Var.Int 1, "Temp_Int_1");
                    (Var.mk_helper Var.Int 1234567, "Temp_Int_1234567");
                  ];
           "Evaluate"
           >::: List.map
                  (fun (testname, expected, expression) ->
                    testname >:: fun _ -> assert_equal_value (OurInt.of_int expected) (evaluate expression))
                  [
                    ("Constant", 42, " 42 ");
                    ("Negated Constant", -42, " - 42 ");
                    ("Variable", 3, " x ");
                    ("Negated Variable", -3, " - x ");
                    ("Parentheses", 3, " ( x ) ");
                    ("Double Parentheses", 3, " ( ( x ) )");
                    ("Constant Addition", 42 + 24, " 42 + 24 ");
                    ("Constant Subtraction", 42 - 24, " 42 - 24 ");
                    ("Constant Multiplication", 42 * 24, " 42 * 24 ");
                    ("Addition", 32, " x + 24 + y ");
                    ("Subtraction", 15, " 42 - 24 - x ");
                    ("Multiplication", 3 * 42 * 24, " 42 * x * 24 ");
                    ("Power", 27, " x ^ 3 ");
                    ("Double Negation", 42, " - - 42 ");
                    ("Multiplication before Addition", 38, " x + y * z ");
                    ("Power before Negation", -27, " - x ^ 3 ");
                    ("Power before Multiplication", 27 * 625, " x ^ 3 * y ^ 4 ");
                  ];
           "Evaluate_partial"
           >::: List.map
                  (fun (testname, expected, poly) ->
                    testname >:: fun _ -> assert_equal_poly expected (evaluate_partial poly))
                  [
                    ("Constant", Polynomial.value 42, Polynomial.value 42);
                    ("Parameter", Polynomial.value 1, Polynomial.int_helper 1);
                    ( "Parameter and Variable",
                      Polynomial.(Polynomial.value 1 + Polynomial.var "x"),
                      Polynomial.(Polynomial.int_helper 1 + Polynomial.var "x") );
                    ( "Parameter as a factor",
                      Polynomial.mul (Polynomial.value 1) (Polynomial.var "x"),
                      Polynomial.mul (Polynomial.int_helper 1) (Polynomial.var "x") );
                    ( "Parameter as a factor vanishes",
                      Polynomial.zero,
                      Polynomial.mul (Polynomial.int_helper 2) (Polynomial.var "x") );
                    ( "Multiple parameters",
                      Polynomial.(Polynomial.var "x" - Polynomial.var "z"),
                      Polynomial.(
                        (Polynomial.int_helper 1 * Polynomial.var "x")
                        + ((Polynomial.int_helper 2 * Polynomial.var "y")
                          + (Polynomial.int_helper 3 * Polynomial.var "z"))) );
                  ];
           "Instantiate"
           >::: List.map
                  (fun (testname, expected, poly) ->
                    testname >:: fun _ ->
                    assert_equal_poly expected (Polynomial.instantiate Polynomial.of_constant poly))
                  [ ("Constant", Polynomial.value 42, Polynomial.value 42) ];
           "Instantiate TemplatePolynomial"
           >::: List.map
                  (fun (testname, expected, poly) ->
                    testname >:: fun _ ->
                    assert_equal_parameter_poly expected
                      (ParameterPolynomial.instantiate
                         (fun v ->
                           ParameterPolynomial.of_constant
                             (Polynomial.of_constant (Polynomial.eval_f v (fun _ -> OurInt.of_int 2))))
                         poly))
                  [
                    ( "a*x",
                      ParameterPolynomial.(value 2 * var "x"),
                      ParameterPolynomial.(of_constant (Polynomial.var "a") * var "x") );
                  ];
           ("Flatten TemplatePolynomial"
           >:::
           let module T = ParameterPolynomial in
           List.map
             (fun (testname, expected, poly) ->
               testname >:: fun _ -> assert_equal_poly expected (T.flatten poly))
             [
               ( "1st",
                 Polynomial.(
                   (value 2 * var "a" * var "x")
                   + (var "b" * var "x")
                   + (value 3 * var "a" * var "y")
                   - value 1),
                 T.(
                   (of_constant Polynomial.((value 2 * var "a") + var "b") * var "x")
                   + (of_constant Polynomial.(value 3 * var "a") * var "y")
                   - value 1) );
             ]);
           "from_coeff_list"
           >::: List.map
                  (fun (expected, coeffs, vars) ->
                    expected >:: fun _ ->
                    assert_equal_poly
                      (Polynomial.of_coeff_list coeffs vars)
                      (Readers.read_polynomial expected))
                  [
                    ( "2 * x + 3 * y - 4 * z",
                      [ of_int 2; of_int 3; of_int (-4) ],
                      [ of_string "x"; of_string "y"; of_string "z" ] );
                    ("0", [ of_int 2; of_int 3 ], [ of_string "x"; of_string "y"; of_string "z" ]);
                    ("0", [ of_int 2; of_int 3; of_int (-4) ], [ of_string "z" ]);
                    ( "(-1) * a + 3 * b - 2 * c",
                      [ of_int (-1); of_int 3; of_int (-2) ],
                      [ of_string "a"; of_string "b"; of_string "c" ] );
                  ];
           "Math"
           >::: [
                  ( "zero" >:: fun _ ->
                    assert_equal_value (OurInt.of_int 0) (Polynomial.eval Polynomial.zero example_valuation)
                  );
                  ( "one" >:: fun _ ->
                    assert_equal_value (OurInt.of_int 1) (Polynomial.eval Polynomial.one example_valuation) );
                  "constant"
                  >::: List.map
                         (fun (expected, expression) ->
                           expression >:: fun _ ->
                           assert_equal_value (OurInt.of_int expected)
                             (Polynomial.get_constant (Readers.read_polynomial expression)))
                         [
                           (5, " 5 ");
                           (0, " x ");
                           (15, " 5 + 2 + x + 8 ");
                           (10, " 5 * 2 + x ");
                           (0, " 5 * 2 * x ");
                           (1, " x ^ 0 ");
                           (15, " 5 * ( x + 3 ) ");
                         ];
                  "is_var"
                  >::: List.map
                         (fun (expected, expression) ->
                           expression >:: fun _ ->
                           assert_equal_bool expected
                             (Polynomial.is_indeterminate (Readers.read_polynomial expression)))
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
                           (true, "x*y*z*a*b*c + d - x*y*z*a*b*c");
                         ];
                  "is_univariate_linear"
                  >::: List.map
                         (fun (expected, expression) ->
                           expression >:: fun _ ->
                           assert_equal_bool expected
                             (Polynomial.is_univariate_linear (Readers.read_polynomial expression)))
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
                           (true, "x*y*z*a*b*c + d - x*y*z*a*b*c");
                         ];
                  "is_linear"
                  >::: List.map
                         (fun (expected, expression) ->
                           expression >:: fun _ ->
                           assert_equal_bool expected
                             (Polynomial.is_linear (Readers.read_polynomial expression)))
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
                           (true, "x*y*z*a*b*c + d - x*y*z*a*b*c");
                           (false, "-10*z^3 + 10 * z^3 + x*x*x*x^0");
                           (true, "x^0 + y^0 + z + f + g");
                           (true, "3");
                           (true, "0");
                         ];
                  "equals"
                  >::: List.map
                         (fun (expected, expression1, expression2) ->
                           expression1 ^ "," ^ expression2 >:: fun _ ->
                           assert_equal_bool expected
                             (Polynomial.equal
                                (Readers.read_polynomial expression1)
                                (Readers.read_polynomial expression2)))
                         [
                           (true, "x + y", "y + x"); (true, "x + y", "x + y"); (true, "x + y - (y + x)", "0");
                         ];
                  (*
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
                 *)
                  "substitute"
                  >::: List.map
                         (fun (expected, substitution, polynomial) ->
                           polynomial >:: fun _ ->
                           assert_equal_poly (Readers.read_polynomial expected)
                             (Polynomial.substitute (Var.of_string "x")
                                ~replacement:(Readers.read_polynomial substitution)
                                (Readers.read_polynomial polynomial)))
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
                  "simplified"
                  >::: List.map
                         (fun (expected, poly, poly_str) ->
                           poly_str >:: fun _ -> assert_equal_poly (Readers.read_polynomial expected) poly)
                         (let poly = Readers.read_polynomial "X+Y" in
                          let poly2 = Readers.read_polynomial "X-Y" in
                          let polyneg = Polynomial.neg poly in
                          let polystr = Polynomial.to_string poly in
                          let poly2str = Polynomial.to_string poly in
                          [
                            ("2*X+2*Y", Polynomial.add poly poly, "(" ^ polystr ^ ")+(" ^ polystr ^ ")");
                            ( "X^2 + 2*X*Y + Y^2",
                              Polynomial.mul poly poly,
                              "(" ^ polystr ^ ")*(" ^ polystr ^ ")" );
                            ( "-2*X*Y - X^2 - Y^2",
                              Polynomial.mul poly polyneg,
                              "(" ^ polystr ^ ")*(-(" ^ polystr ^ "))" );
                            ( "-2*X*Y - X^2 - Y^2",
                              Polynomial.mul polyneg poly,
                              "(-(" ^ polystr ^ "))*(" ^ polystr ^ ")" );
                            ( "2*X*Y + X^2 + Y^2",
                              Polynomial.mul polyneg polyneg,
                              "(-(" ^ polystr ^ "))*(-(" ^ polystr ^ "))" );
                            ( "X*X - 2*X*Y + Y^2",
                              Polynomial.mul poly2 poly2,
                              "(-(" ^ poly2str ^ "))*(-(" ^ poly2str ^ "))" );
                            ( "X*X - Y^2",
                              Polynomial.mul poly2 poly,
                              "(-(" ^ poly2str ^ "))*(-(" ^ polystr ^ "))" );
                            ( "X*X - Y^2",
                              Polynomial.mul poly poly2,
                              "(-(" ^ polystr ^ "))*(-(" ^ poly2str ^ "))" );
                          ]);
                ];
         ]
end
