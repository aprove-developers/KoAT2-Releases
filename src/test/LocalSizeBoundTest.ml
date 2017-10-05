open Batteries
open OUnit2
open Helper
open LocalSizeBound
open Formulas
   
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

      ("find_bound" >:::
         List.map (fun (expected_classification, expected_vars, guard) ->
             "bound for x with " ^ guard >::
               (fun _ -> assert_equal_classified_bound
                           (mk expected_classification expected_vars)
                           (find_bound (Var.of_string "x") (Readers.read_formula guard))))
                  [
                    (Equality 5, [], "x <= 5");
                    (AddsConstant 0, ["y"], "x <= y");
                    (Equality 10, [], "x <= y && y <= 10");
                    (* Not solvable yet (Equality 0, ["z"], "x", "x <= y && y <= z"); *)
                  ]
      );

    ]
