open Koat2
open Batteries
open OUnit2
open Atoms
open Polynomials
open Helper

module Parser = struct
  let assert_equal_atom = assert_equal ~cmp:Atom.( =~= ) ~printer:Atom.to_string

  let tests =
    "Parser"
    >::: [
           ("Positive Tests"
           >:::
           let open Polynomial in
           let open Atom.Infix in
           List.map
             (fun (testname, expected, atom) ->
               testname >:: fun _ -> assert_equal_atom expected (Readers.read_atom atom))
             [
               ("Constants LT", value 42 < value 42, " 42 < 42 ");
               ("Constants LE", value 42 <= value 42, " 42 <= 42 ");
               ("Constants GT", value 42 > value 42, " 42 > 42 ");
               ("Constants GE", value 42 >= value 42, " 42 >= 42 ");
               ( "Constant and Poly LT",
                 value 42 < (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " 42 < x^2+ 5*x*y*z " );
               ( "Constant and Poly LE",
                 value 42 <= (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " 42 <= x^2+ 5*x*y*z " );
               ( "Constant and Poly GT",
                 value 42 > (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " 42 > x^2+ 5*x*y*z " );
               ( "Constant and Poly GE",
                 value 42 >= (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " 42 >= x^2+ 5*x*y*z " );
               ( "Poly and Poly LT",
                 (var "x" ** 5) + (var "y" ** 6) - (var "z" ** 3)
                 < (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " x^5+y^6-z^3 < x^2+ 5*x*y*z " );
               ( "Poly and Poly LE",
                 (var "x" ** 5) + (var "y" ** 6) - (var "z" ** 3)
                 <= (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " x^5+y^6-z^3 <= x^2+ 5*x*y*z " );
               ( "Poly and Poly GT",
                 (var "x" ** 5) + (var "y" ** 6) - (var "z" ** 3)
                 > (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " x^5+y^6-z^3 > x^2+ 5*x*y*z " );
               ( "Poly and Poly GE",
                 (var "x" ** 5) + (var "y" ** 6) - (var "z" ** 3)
                 <= (var "x" ** 2) + (value 5 * var "x" * var "y" * var "z"),
                 " x^5+y^6-z^3 >= x^2+ 5*x*y*z " );
             ]);
           "Negative Tests"
           >::: List.map
                  (fun (testname, atom) ->
                    testname >:: fun _ ->
                    assert_raises (Lexer.SyntaxError testname) (fun _ -> Readers.read_atom atom))
                  [ ("Unexpected char: =", "x = y") ];
         ]
end

module Methods = struct
  let rename str rename_map =
    str |> Readers.read_atom |> fun atom -> Atom.rename atom (RenameMap.from_native rename_map)


  (*
    let evaluate str valuation =
         str
      |> Reader.read_atom
      |> fun atom -> Atom.models atom (Polynomial.Valuation_.from_native valuation)
                    *)

  let tests =
    "ConstraintsAtom"
    >::: [
           "(=~=)"
           >::: List.map
                  (fun (atom1, atom2) ->
                    atom1 ^ "=~=" ^ atom2 >:: fun _ ->
                    assert_equal_atom (Readers.read_atom atom1) (Readers.read_atom atom2))
                  [
                    ("x < y", "y > x");
                    ("x <= y", "y >= x");
                    ("x <= y", "x - 1 < y");
                    ("x > y", "x - 1 >= y");
                    (*("4*x >= 2*y", "2*x >= y");*)
                    (* Those are equivalent, but we can not decide (yet):
                       ("2*x < 0", "x < 0");)
                       ("4*x > 2*y", "2*x > y");
                    *)
                    ("x*y < x", "x * (y - 1) < 0");
                  ];
           "vars"
           >::: List.map
                  (fun (expected, atom) ->
                    atom >:: fun _ ->
                    assert_equal_varset (VarSet.of_string_list expected) (Atom.vars (Readers.read_atom atom)))
                  [
                    ([ "x" ], " x^3+2*x -1 < x^5 ");
                    ( [ "x"; "y"; "z" ],
                      " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z " );
                  ];
           "rename"
           >::: List.map
                  (fun (expected, atom) ->
                    atom >:: fun _ ->
                    assert_equal_atom (Readers.read_atom expected)
                      (rename atom [ ("x", "a"); ("y", "b"); ("z", "c") ]))
                  [
                    ("5 <= 5", "5 <= 5");
                    ("a <= a", "x <= x");
                    ("a <= b", "x <= y");
                    ("a < a ^ 2 + 2 * a * b", "x < x ^ 2 + 2 * x * y");
                    ("a^2 * b^2 < 7", "x^2 * y^2 < 7");
                  ];
           (*
            ("models" >:::
                List.map (fun (expected, atom, valuation) ->
                      atom >:: (fun _ ->  assert_equal_bool expected (evaluate atom valuation)))
                        [
                            (true, " 5 <= 5 ", [("x", 3)]);
                            (true, "x < x ^ 2 + 2 * x * y", [("x", 3); ("y", 5)]);
                            (false, "x^2 * y^2 < 7", [("x", 3); ("y", 5); ("z", 7)]);
                        ]);
             *)
           "is_linear"
           >::: List.map
                  (fun (expected, atom) ->
                    atom >:: fun _ -> assert_equal_bool expected (Atom.is_linear (Readers.read_atom atom)))
                  [ (true, "x < y"); (false, "x <= a^2 + b * 3 -6"); (false, "x >= a^2 + b * 3 -6") ];
           "negation"
           >::: List.map
                  (fun (expected, atom) ->
                    atom >:: fun _ ->
                    assert_equal_atom (Readers.read_atom expected) (Readers.read_atom atom |> Atom.neg))
                  [ ("x <= 0", "x > 0"); ("x >= y", "x < y"); ("1 < 0", "0 <= 1"); ("1 > 0", "0 >= 1") ];
         ]
end
