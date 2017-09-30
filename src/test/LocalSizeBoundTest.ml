open Batteries
open OUnit2
open Helper
open LocalSizeBound
   
let tests = 
  "LocalSizeBound" >::: [
                        
      ("as_bound" >:::
         List.map (fun (expected, classification) ->
             "as_bound-Test" >:: (fun _ -> assert_equal_bound (Readers.read_bound expected) (as_bound classification)))
                  [
                    ("max{5}", (Equality 5, []));
                    ("max{3, x}", (Equality 3, [Var.of_string "x"]));
                    ("max{-2, x, y}", (Equality (-2), [Var.of_string "x"; Var.of_string "y"]));
                  ]
      );

      ("is_bounded_with" >:::
         List.map (fun (formula, classification) ->
             "as_bound-Test" >:: (fun _ -> assert_true (is_bounded_with (Var.of_string "x") (Readers.read_formula formula) classification)))
                  [
                    ("x = 5", (Equality 5, []));
                    ("x = y", (Equality 0, [Var.of_string "y"]));
                  ]
      );

      ("find_bound" >:::
         List.map (fun (expected, var, guard) ->
             "bound for " ^ var ^ " with " ^ guard >:: (fun _ -> assert_equal_classified_bound expected (find_bound (Var.of_string var) (Readers.read_formula guard))))
                  [
                    ((Equality 5, []), "x", "x = 5");
                    ((Equality 0, [Var.of_string "y"]), "x", "x = y");
                  ]
      );

    ]
