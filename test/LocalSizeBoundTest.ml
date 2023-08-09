open Koat2
open Batteries
open OUnit2
open Helper
open LocalSizeBound
open Formulas
open Polynomials

let tests =
  "LocalSizeBound"
  >::: [
         "find upper bound"
         >::: List.map
                (fun (expected, guard, exp_equality) ->
                  "bound for x with " ^ guard >:: fun _ ->
                  let bound =
                    find_bound
                      (VarSet.of_list [ Var.of_string "y"; Var.of_string "z" ])
                      (Var.of_string "x") (Readers.read_formula guard) 1024
                  in
                  match bound with
                  | Some (lsb, equality) ->
                      assert_equal_bound (LocalSizeBound.as_bound expected) (LocalSizeBound.as_bound lsb);
                      assert_equal_bool exp_equality (Lazy.force equality)
                  | _ -> assert_equal_bound (LocalSizeBound.as_bound expected) Bounds.Bound.infinity)
                [
                  (* Bounded by constants *)
                  (mk ~c:5 [], "x <= 0 && x >= -5", true);
                  (mk ~c:5 [], "x <= 5 && x >= -2", true);
                  (mk ~c:3 [], "x = 7 - 10", true);
                  (mk ~c:7 [], "x <= 7 + y - y && x>= 0", true);
                  (mk ~c:10 [], "x <= y && y <= 10 && x>=-2", true);
                  (mk ~c:15 [], "x <= y + 7 && y <= 8 && x>=0", true);
                  (mk ~c:10 [], "x <= y && x <= 10 && x>=0", true);
                  (* Bounded by variable *)
                  (mk [ "y" ], "x <= y && x>=-y", true);
                  (mk [ "y" ], "x <= y && x >= y", true);
                  (mk [ "y" ], "x <= y && x > 5", true);
                  (mk [ "y" ], "x = y", true);
                  (* TODO Better heuristic for optimize vars: (AddsConstant 0 ["y"] "x <= y && y <= z"); *)
                  (mk [ "z" ], "x <= z && y <= z && x>=0", true);
                  (* Bounded by constant plus variable *)
                  (mk ~c:5 [ "y" ], "x <= y + 5 && x>=-y -5", false);
                  (mk ~c:0 [ "y" ], "x <= y - 5 && x>=-y+5", true);
                  (mk ~c:2 [ "y" ], "x <= y + z - 5 && z <= 7 && x>=0", false);
                  (* With factor and positive coefficients *)
                  (mk ~s:2 [ "y" ], "x <= 2*y && x>=0", false);
                  (mk ~s:2 [ "y" ], "x <= 2*y && x>=-2*y", false);
                  (mk ~s:2 [ "y"; "z" ], "x <= 2*y + 2*z && x> -2*y - 2*z", false);
                  (mk ~s:3 [ "y"; "z" ], "x <= 2*y + 3*z && x>= -2*y - 3*z", false);
                  (* Bounded by two variables but of equality type *)
                  (mk ~s:1 [ "y"; "z" ], "y >= 0 && z<0 && x = y+z", true);
                ];
       ]
