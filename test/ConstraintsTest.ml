open Koat2
open Batteries
open OUnit2
open Helper
open Constraints
open Polynomials

module Parser = struct
  let tests =
    "Parser"
    >::: [
           ("All together"
           >:::
           let open Polynomial in
           let open Constraint.Infix in
           List.map
             (fun (testname, expected, atom) ->
               testname >:: fun _ -> assert_equal_constr expected (Readers.read_constraint atom))
             [
               ( "Constants ",
                 value 42 < value 42
                 && value 1 >= value 0
                 && value 2 <= value 4
                 && value 6 <= value 7
                 && value 7 <= value 6,
                 " 42 < 42 && 1 >= 0 && 2 <= 4 && 6 = 7" );
               ("Constants and Variables", var "x" > value 0 && var "y" < value 3, "x > 0 && y < 3");
             ]);
           "Negative Tests"
           >::: List.map
                  (fun (testname, atom) ->
                    testname >:: fun _ -> assert_exception (fun _ -> Readers.read_constraint atom))
                  [ ("Unexpected char: =", "x == y") ];
         ]
end

module Methods = struct
  module Valuation = Valuation.Make (OurInt)

  let example_valuation = Valuation.from_native [ ("x", 3); ("y", 5); ("z", 7) ]
  let example_renaming = RenameMap.from_native [ ("x", "a"); ("y", "b"); ("z", "c") ]
  let rename str = str |> Readers.read_constraint |> fun constr -> Constraint.rename constr example_renaming

  (*
    let evaluate str =
         str
      |> Reader.read_constraint
      |> fun constr -> C.models constr example_valuation
                      *)

  let assert_equal_constr = assert_equal ~cmp:Constraint.( =~= ) ~printer:Constraint.to_string
  let of_int = OurInt.of_int

  let rec list_equality (xs : OurInt.t list) (ys : OurInt.t list) =
    match (xs, ys) with
    | [], [] -> true
    | x :: tailxs, y :: tailys -> OurInt.(x =~= y) && list_equality tailxs tailys
    | _, _ -> false


  let list_print (xs : OurInt.t list) = "[" ^ String.concat "," (List.map OurInt.to_string xs) ^ "]"
  let list_list_equality xs ys = list_equality (List.concat xs) (List.concat ys)
  let list_list_print xs = xs |> List.map list_print |> String.concat ","
  let print_str (str : string) = str

  let tests =
    "Constraints"
    >::: [
           (*let open Program.Constraint_.Atom_.Polynomial_ in
              let open Program.Constraint_ in*)
           "get_variables"
           >::: List.map
                  (fun (expected, constr) ->
                    constr >:: fun _ ->
                    assert_equal_varset (VarSet.of_string_list expected)
                      (Constraint.vars (Readers.read_constraint constr)))
                  [
                    ( [ "x"; "y"; "z" ],
                      " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z && x > 0 \
                       && y >= 0 && z <= 4" );
                  ];
           "rename_vars"
           >::: List.map
                  (fun (expected, constr) ->
                    constr >:: fun _ -> assert_equal_constr (Readers.read_constraint expected) (rename constr))
                  [
                    ("5 <= 5", " 5 <= 5 ");
                    ("a <= a", "x <= x");
                    ("a < a ^ 2 + 2 * a * b", "x < x ^ 2 + 2 * x * y");
                    ("a^2 * b^2 < 7", "x^2 * y^2 < 7");
                    ("a <= a && b < c", "x <= x && y < z");
                  ];
           (* ("models" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ ->  assert_equal_bool expected (evaluate constr )))
                        [
                            (true, " 5 <= 5 " );
                            (true, "x <= x" );
                            (true,"x < x ^ 2 + 2 * x * y" );
                            (false, "x^5+y^6-z^3 = x * y * z + x^2 ");
                            (false , "x^2 * y^2 < 7");
                            (true , "2 < 3 && 3 < 4 && 4 < 5");
                            (false, "3 <= 3 && 2 <= 2 && 1 <= 0");
                        ]);*)
           "drop_nonlinear"
           >::: List.map
                  (fun (expected, constr) ->
                    constr >:: fun _ ->
                    assert_equal_constr (Readers.read_constraint expected)
                      (Constraint.drop_nonlinear (Readers.read_constraint constr)))
                  [
                    ("", "x^2 < x*y + 3");
                    ("3 < x", "3 < x + y^3 - y*y^2");
                    ("x <= y && y <= z", "x <= y && y <= z");
                  ];
           "get_coefficient_vector"
           >::: List.map
                  (fun (expected, var, constr) ->
                    constr >:: fun _ ->
                    assert_equal ~cmp:list_equality ~printer:list_print (List.map OurInt.of_int expected)
                      (Constraint.get_coefficient_vector
                         (var |> Var.of_string |> Polynomial.of_var |> Polynomial.monomials |> List.first)
                         (Readers.read_constraint constr)))
                  [
                    ([ 1; 2; 3 ], "x", "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                    ([ 1; 3; -4 ], "y", "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                    ([ 0; 0; 0 ], "z", "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                    ( [ 3; 1; 7; -7 ],
                      "x",
                      "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1" );
                    ( [ 2; 3; 0; 0 ],
                      "y",
                      "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1" );
                    ( [ 4; 0; 3; -3 ],
                      "z",
                      "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1" );
                  ];
           "get_constant_vector"
           >::: List.map
                  (fun (expected, constr) ->
                    constr >:: fun _ ->
                    assert_equal ~cmp:list_equality ~printer:list_print (List.map OurInt.of_int expected)
                      (Constraint.get_constant_vector (Readers.read_constraint constr)))
                  [
                    ([ 5; -2; 0 ], "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                    ([ 8; -4; 1; -1 ], "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1");
                    ([ 0 ], "2 *x + y <= 0");
                  ];
           "get_matrix"
           >::: List.map
                  (fun (expected, vars, constr) ->
                    constr >:: fun _ ->
                    assert_equal ~cmp:list_list_equality ~printer:list_list_print
                      (List.map (List.map OurInt.of_int) expected)
                      (Constraint.get_matrix
                         (List.map
                            (fun var ->
                              var |> Var.of_string |> Polynomial.of_var |> Polynomial.monomials |> List.first)
                            vars)
                         (Readers.read_constraint constr)))
                  [
                    ( [ [ 1; 2; 3 ]; [ 1; 3; -4 ] ],
                      [ "x"; "y" ],
                      "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0" );
                    ([ [ 1; -1 ]; [ -1; 1 ] ], [ "x"; "y" ], "x = y");
                    ( [ [ -1; 0; 0; -1 ]; [ 0; -1; 1; -1 ]; [ 1; 1; -1; -1 ] ],
                      [ "x"; "y"; "z" ],
                      "x > z && z = y && x + y + z > 3" );
                    ([ [ 2 ]; [ 1 ] ], [ "x"; "y" ], "2 *x + y <= 0");
                    ( [ [ 1; 1; -1; 0 ]; [ 1; 0; 0; -1 ] ],
                      [ "x"; "y" ],
                      "x + y <= 4 && x <= 3 && x >= 0 && y>=0" );
                  ];
           (*("farkas_transform" >:::
               let open Polynomial in
               let open Constraint in
               let open Constraint.Infix in
                   let assert_equal_constr = assert_equal ~cmp:Constraint.(=~=) ~printer:Constraint.to_string in
               List.map (fun (expected, constr, atom) ->
                     (Atom.to_string atom) >:: (fun _ -> assert_equal_constr expected (Constraint.farkas_transform constr atom )))
                       [
                         ( (all [ (((value 1)*(real_helper 1)) + ((value 1)*(real_helper 2)) + ((value (-1))*(real_helper 3))) = value 2;
                                  (((value 1)*(real_helper 1)) + ((value (-1))*(real_helper 4))) = value 1;
                                  real_helper 1 >= value 0;
                                  real_helper 2 >= value 0;
                                  real_helper 3 >= value 0;
                                  real_helper 4 >= value 0;
                                  (value 4)*(real_helper 1) + (value 3) * (real_helper 2) <= value 0
                           ]),
                           (all [(var "x")+(var "y") <= value 4;
                                 var "x" <= value 3;
                                 var "x" >= value 0;
                                 var "y" >= value 0
                           ]),
                           Atom.Infix.(((value 2) * (var "x")) + (var "y") <= value 0));

                           (all ([mk_eq ((value (-1))*(real_helper 1))(value (-1));mk_ge (real_helper 1) (value 0);mk_le (value 0) (value 0)]),
                           (all [mk_ge (var "x") (value 0)]),
                           Atom.mk_ge (var "x") (value 0));
                       ]);*)
         ]
end
