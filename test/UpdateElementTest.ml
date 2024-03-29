open Koat2
open! OurBase
open OUnit2
open Helper
open Formulas
open ProbabilisticProgramModules

let tests =
  "UpdateElementTests"
  >::: [
         "check_update_as_guard_of_max_degree"
         >::: List.mapi
                ~f:(fun i (guard, updated_var, update, expected_result) ->
                  "case " ^ Int.to_string i >:: fun _ ->
                  let guard = Readers.read_constraint guard in
                  let updated_var = Var.of_string updated_var in
                  let update = Readers.read_update_element update in
                  let expected_result = Readers.read_constraint expected_result in
                  let result = UpdateElement.as_linear_guard guard update updated_var in

                  assert_equal_formula (Formula.mk expected_result) (Formula.mk result))
                [
                  ("X>=1", "X'", "X*X", "X >= 1 && X' >= X");
                  ("X<=(-1)", "X'", "X*X", "X <= (-1) && X' >= -X");
                  ("X>=2", "X'", "X*X", "X >= 2 && X' >= 2*X");
                  ("X<=(-2)", "X'", "X*X", "X <= (-2) && X' >= -2*X");
                  ("X<=0", "X'", "X*X", "X <= 0 && X' >= X && X' >= 0");
                  ("X>=0", "X'", "X*X", "X >= 0 && X' >= 0") (* imprecise *);
                  ("", "X'", "UNIFORM(3,4)", "3 <= X' && X' <= 4");
                  ("", "X'", "UNIFORM(-3,4)", "(-3) <= X' && X' <= 4");
                  ("", "X'", "UNIFORM(3,4)^2", "9 <= X' && X' <= 16");
                  ("", "X'", "UNIFORM(-3,4)^2", "0 <= X' && X' <= 16");
                  ( "X<=10",
                    "X'",
                    "BINOMIAL(X,0.5) * UNIFORM(1,3)",
                    "0 <= X && X <= 10 && 0 <= X' && X'<= 10+2*X" )
                  (* imprecise due to overapproximation *);
                  ( "X<=Y",
                    "X'",
                    "BINOMIAL(X,0.5) * UNIFORM(1,3)",
                    "X<=Y && 0 <= X' && X' <= 3*X && X>=0 && Y>=0" );
                  ("X<=Y", "X'", "BINOMIAL(X,0.5) * UNIFORM(1,3)^2", "X<=Y && 0 <= X' && X'<=9*X");
                  ("X<=Y", "X'", "BINOMIAL(X,0.5) * UNIFORM(-1,3)^2", "X<=Y && 0 <= X' && X'<=9*X");
                  ("", "X'", "BINOMIAL(10,0.5) * UNIFORM(-3,4)^2", "0 <= X' && X' <= 160");
                  ("X<=Y", "X'", "BINOMIAL(X,0.5) * UNIFORM(-3,4)^2", "X<=Y && 0 <= X' && X' <= 16*X")
                  (* imprecise due to overapproximation *);
                ];
         "check_exp_value"
         >::: List.map
                ~f:(fun (ue_str, exp_result) ->
                  ue_str >:: fun _ ->
                  let ue = Readers.read_update_element ue_str in
                  let exp_value = UpdateElement.exp_value_poly ue in
                  assert_equal_rationalpoly_smt exp_result exp_value)
                (let open Polynomials.RationalPolynomial in
                 [
                   ("UNIFORM(0,3)", of_constant (OurRational.of_float 1.5));
                   ("UNIFORM(0,3)*UNIFORM(0,3)", of_constant (OurRational.of_float (1.5 *. 1.5)));
                   ("UNIFORM(0,3)^2", of_constant (OurRational.of_float (14. /. 4.)));
                 ]);
       ]
