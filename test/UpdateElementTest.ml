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
                  let guard = KoatReaders.read_constraint guard in
                  let updated_var = Var.of_string updated_var in
                  let update = KoatReaders.read_update_element update in
                  let expected_result = KoatReaders.read_constraint expected_result in
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
                  let ue = KoatReaders.read_update_element ue_str in
                  let exp_value = UpdateElement.exp_value_poly ue in
                  assert_equal_rationalpoly_smt exp_result exp_value)
                (let open Polynomials.RationalPolynomial in
                 [
                   ("UNIFORM(0,3)", of_constant (OurRational.of_float 1.5));
                   (* Every occurrence of a distribution is an independent sample *)
                   ("UNIFORM(0,3)*UNIFORM(0,3)", of_constant (OurRational.of_float (1.5 *. 1.5)));
                   ("UNIFORM(1,2)*UNIFORM(1,2)", of_constant (OurRational.of_float (1.5 *. 1.5)));
                   ("0:UNIFORM(0,3)*1:UNIFORM(0,3)", of_constant (OurRational.of_float (1.5 *. 1.5)));
                   (* A single sample *)
                   ("UNIFORM(0,3)^2", of_constant (OurRational.of_float (14. /. 4.)));
                   ("(UNIFORM(0,3))^2", of_constant (OurRational.of_float (14. /. 4.)));
                   ("(UNIFORM(1,2))^2", of_constant (OurRational.of_float 2.5));
                   ("1:UNIFORM(0,3)*1:UNIFORM(0,3)", of_constant (OurRational.of_float (14. /. 4.)));
                   (* Powers bind stronger than the other operators *)
                   ( "(X + Y)^2",
                     let x = of_var (Var.of_string "X") and y = of_var (Var.of_string "Y") in
                     add (add (pow x 2) (mul (of_int 2) (mul x y))) (pow y 2) );
                   ("X + Y^2", add (of_var (Var.of_string "X")) (pow (of_var (Var.of_string "Y")) 2));
                   ("-X^2", neg (pow (of_var (Var.of_string "X")) 2));
                   ("2*UNIFORM(0,3)^2", of_constant (OurRational.of_float (2. *. 14. /. 4.)));
                   ( "(X + UNIFORM(0,3))^2",
                     let x = of_var (Var.of_string "X") in
                     add (add (pow x 2) (mul (of_int 3) x)) (of_constant (OurRational.of_float (14. /. 4.)))
                   );
                 ]);
         ( "same_label_id_requires_same_distribution" >:: fun _ ->
           match KoatReaders.read_update_element "1:UNIFORM(0,3) + 1:GEOMETRIC(0.5)" with
           | exception KoatParserUtil.Error _ -> ()
           | _ -> assert_failure "the same label id for different distributions must be rejected" );
         (* The ids are distinct for all updates of a general transition, apart from the same label ids. To observe
            this, the expected value of the product of the updates of the two arguments is computed. *)
         "distribution_ids_in_general_transitions"
         >::: List.map
                ~f:(fun (gt_str, expected) ->
                  gt_str >:: fun _ ->
                  let gt = Set.choose_exn (KoatReaders.read_general_transitions gt_str) in
                  let label = Tuple3.second (Set.choose_exn (GeneralTransition.transitions gt)) in
                  let update var = Option.value_exn (TransitionLabel.update label var) in
                  let product = Sequence.take Var.args 2 |> Sequence.map ~f:update |> UpdateElement.product in
                  assert_equal_rationalpoly_smt
                    (Polynomials.RationalPolynomial.of_constant (OurRational.of_float expected))
                    (UpdateElement.exp_value_poly product))
                [
                  ("(RULES f(X,Y) -> g(UNIFORM(0,3), UNIFORM(0,3)))", 1.5 *. 1.5);
                  ("(RULES f(X,Y) -> g(1:UNIFORM(0,3), 2:UNIFORM(0,3)))", 1.5 *. 1.5);
                  ("(RULES f(X,Y) -> g(1:UNIFORM(0,3), 1:UNIFORM(0,3)))", 14. /. 4.);
                  ("(RULES f(X,Y) -> g(UNIFORM(0,3), 1:UNIFORM(0,3)))", 1.5 *. 1.5);
                ];
       ]
