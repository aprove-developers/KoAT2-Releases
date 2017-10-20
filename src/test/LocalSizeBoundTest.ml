open Batteries
open OUnit2
open Helper
open LocalSizeBound
open Formulas
   
let tests = 
  "LocalSizeBound" >::: [
                        
      ("as_bound" >:::
         List.map (fun (expected, template_bound) ->
             "as_bound-Test" >::
               (fun _ -> assert_equal_bound
                           (Readers.read_bound expected)
                           (as_bound (Some template_bound))))
                  [
                    ("5", ScaledSum (1, 5, VarSet.empty));
                    ("3 + x", ScaledSum(1, 3, VarSet.of_string_list ["x"]));
                    ("-2 + x + y", ScaledSum (1, (-2), VarSet.of_string_list ["x"; "y"]));
                  ]
      );

      ("is_bounded_with" >:::
         List.map (fun (formula, template_bound) ->
             "as_bound-Test" >::
               (fun _ -> assert_true (is_bounded_with (Var.of_string "x") (Readers.read_formula formula) template_bound)))
                  [
                    ("x = 5", ScaledSum (1, 5, VarSet.empty));
                    ("x = y", ScaledSum (1, 0, VarSet.of_string_list ["y"]));
                  ]
      );

      ("find_bound" >:::
         List.map (fun (expected, guard) ->
             "bound for x with " ^ guard >::
               (fun _ -> assert_equal_classified_bound
                           expected
                           (Option.get (find_bound (Var.of_string "x") (Readers.read_formula guard)))))
                  [
                    (* Bounded by constants *)
                    (ScaledSum (1, 0, VarSet.empty), "x <= 0");
                    (ScaledSum (1, 5, VarSet.empty), "x <= 5");
                    (ScaledSum (1, (-5), VarSet.empty), "x <= -5");
                    (ScaledSum (1, (-3), VarSet.empty), "x <= 7 - 10");
                    (ScaledSum (1, 7, VarSet.empty), "x <= 7 + y - y");
                    (ScaledSum (1, 10, VarSet.empty), "x <= y && y <= 10");
                    (ScaledSum (1, 15, VarSet.empty), "x <= y + 7 && y <= 8");
                    (ScaledSum (1, 10, VarSet.empty), "x <= y && x <= 10");
                    (* Bounded by variable *)
                    (ScaledSum (1, 0, VarSet.of_string_list ["y"]), "x <= y");
                    (ScaledSum (1, 0, VarSet.of_string_list ["y"]), "x <= y && x > 5");
                    (* TODO Better heuristic for optimize vars: (AddsConstant 0, ["y"], "x <= y && y <= z"); *)
                    (ScaledSum (1, 0, VarSet.of_string_list ["z"]), "x <= z && y <= z");
                    (* Bounded by constant plus variable *)
                    (ScaledSum (1, 5, VarSet.of_string_list ["y"]), "x <= y + 5");
                    (ScaledSum (1, (-5), VarSet.of_string_list ["y"]), "x <= y - 5");
                    (ScaledSum (1, 2, VarSet.of_string_list ["y"]), "x <= y + z - 5 && z <= 7");
                  ]
      );

    ]
