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
                    (* Bounded by constants *)
                    (Equality 0, [], "x <= 0");
                    (Equality 5, [], "x <= 5");
                    (Equality (-5), [], "x <= -5");
                    (Equality (-3), [], "x <= 7 - 10");
                    (Equality 7, [], "x <= 7 + y - y");
                    (Equality 10, [], "x <= y && y <= 10");
                    (Equality 15, [], "x <= y + 7 && y <= 8");
                    (Equality 10, [], "x <= y && x <= 10");
                    (* Bounded by variable *)
                    (AddsConstant 0, ["y"], "x <= y");
                    (AddsConstant 0, ["y"], "x <= y && x > 5");
                    (* TODO Better heuristic for optimize vars: (AddsConstant 0, ["y"], "x <= y && y <= z"); *)
                    (AddsConstant 0, ["z"], "x <= z && y <= z");
                    (* Bounded by constant and variable *)
                    (Equality 5, ["y"], "x <= y || x <= 5");                    
                    (Equality 5, ["y"; "z"], "x <= y || x <= 5 || x <= z");
                    (* Bounded by constant plus variable *)
                    (AddsConstant 5, ["y"], "x <= y + 5");
                    (AddsConstant (-5), ["y"], "x <= y - 5");
                    (AddsConstant 2, ["y"], "x <= y + z - 5 && z <= 7");
                  ]
      );

    ]
