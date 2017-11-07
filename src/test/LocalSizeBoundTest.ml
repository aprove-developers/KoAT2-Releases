open Batteries
open OUnit2
open Helper
open LocalSizeBound
open Formulas
open LocalSizeBound
   
let tests = 
  "LocalSizeBound" >::: [
                        
      ("is_bounded_with" >:::
         List.map (fun (formula, template_bound) ->
             "as_bound-Test" >::
               (fun _ -> assert_true (is_bounded_with (Var.of_string "x") (Readers.read_formula formula) template_bound)))
                  [
                    ("x = 5", mk 1 5 [] []);
                    ("x = y", mk 1 0 ["y"] []);
                  ]
      );

      ("find_bound" >:::
         List.map (fun (expected, guard) ->
             "bound for x with " ^ guard >::
               (fun _ -> find_bound `Upper (Var.of_string "x") (Readers.read_formula guard)
                         |> Option.map (fun bound -> fun () -> assert_equal_classified_bound expected bound)
                         |? (fun () -> assert_failure "No bound")
                         |> fun f -> f () ))
                  [
                    (* Bounded by constants *)
                    (mk 1 0 [] [], "x <= 0");
                    (mk 1 5 [] [], "x <= 5");
                    (mk 1 (-5) [] [], "x <= -5");
                    (mk 1 (-3) [] [], "x <= 7 - 10");
                    (mk 1 7 [] [], "x <= 7 + y - y");
                    (mk 1 10 [] [], "x <= y && y <= 10");
                    (mk 1 15 [] [], "x <= y + 7 && y <= 8");
                    (mk 1 10 [] [], "x <= y && x <= 10");
                    (* Bounded by variable *)
                    (mk 1 0 [] ["y"], "x <= y");
                    (mk 1 0 [] ["y"], "x <= y && x >= y");
                    (mk 1 0 [] ["y"], "x <= y && x > 5");
                    (* TODO Better heuristic for optimize vars: (AddsConstant 0 ["y"] "x <= y && y <= z"); *)
                    (mk 1 0 [] ["z"], "x <= z && y <= z");
                    (* Bounded by constant plus variable *)
                    (mk 1 5 [] ["y"], "x <= y + 5");
                    (mk 1 (-5) [] ["y"], "x <= y - 5");
                    (mk 1 2 [] ["y"], "x <= y + z - 5 && z <= 7");
                    (* With factor *)
                    (mk 2 0 [] ["y"], "x <= 2*y");
                    (mk 2 0 [] ["y"; "z"], "x <= 2*y + 2*z");
                    (mk 3 0 ["y"] ["z"], "x <= 2*y + 3*z");
                    (mk 3 0 ["y"; "z"] [], "x <= 2*y + (-3)*z");
                    (mk 2 0 [] ["y"], "x <= 2*y + (-3)*z && z >= 0");
                    (mk 3 0 ["z"] [], "x <= 2*y + (-3)*z && y <= 0");
                  ]
      );

    ]
