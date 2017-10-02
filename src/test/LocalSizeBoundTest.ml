open Batteries
open OUnit2
open Helper
open LocalSizeBound

module VarSet = Set.Make(Var)
module Formula = Formula.PolynomialFormula
              
let tests = 
  "LocalSizeBound" >::: [
                        
      ("as_bound" >:::
         List.map (fun (expected, classification, vars) ->
             "as_bound-Test" >::
               (fun _ -> assert_equal_bound
                           (Readers.read_bound expected)
                           (as_bound (mk classification vars))))
                  [
                    ("max{5}", Equality 5, []);
                    ("max{3, x}", Equality 3, ["x"]);
                    ("max{-2, x, y}", Equality (-2), ["x"; "y"]);
                  ]
      );

      ("is_bounded_with" >:::
         List.map (fun (formula, classification, vars) ->
             "as_bound-Test" >::
               (fun _ -> assert_true (is_bounded_with (Var.of_string "x") (Readers.read_formula formula) (mk classification vars))))
                  [
                    ("x = 5", Equality 5, []);
                    ("x = y", Equality 0, ["y"]);
                  ]
      );

      ("find_equality_bound" >:::
         List.map (fun (expected_c, expected_vars, guard) ->
             let formula = Readers.read_formula guard
             and var = Var.of_string "x" in
             "equality bound for x with " ^ guard >::
               (fun _ -> assert_equal_template_bound_option
                           (Some (mk (Equality expected_c) expected_vars))
                           (find_equality_bound (VarSet.remove var (Formula.vars formula)) var formula)))
                  [
                    (5, [], "x <= 5");
                    (0, ["y"], "x <= y");
                    (* Not solvable yet (10, [], "x <= y && y <= 10"); *)
                    (* Not solvable yet (Equality 0, ["z"], "x", "x <= y && y <= z"); *)
                  ]
      );

      ("find_bound" >:::
         List.map (fun (expected_classification, expected_vars, guard) ->
             "bound for x with " ^ guard >::
               (fun _ -> assert_equal_classified_bound
                           (mk expected_classification expected_vars)
                           (find_bound (Var.of_string "x") (Readers.read_formula guard))))
                  [
                    (Equality 5, [], "x <= 5");
                    (Equality 0, ["y"], "x <= y");
                    (Equality 10, [], "x <= y && y <= 10");
                    (* Not solvable yet (Equality 0, ["z"], "x", "x <= y && y <= z"); *)
                  ]
      );

    ]
