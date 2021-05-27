open Batteries
open OUnit2
open Helper
open LocalSizeBound
open Formulas
open Polynomials
   
let tests = 
  "LocalSizeBound" >::: [
                        
      ("find upper bound" >:::
         List.map (fun (expected, guard) ->
             "bound for x with " ^ guard >::
               (fun _ -> let bound = find_bound
                                                (VarSet.of_list [Var.of_string "y"; Var.of_string "z"])
                                                (Var.of_string "x")
                                                (Readers.read_formula guard)
                                                1024
                         in
                         reset ();
                         assert_equal_bound (LocalSizeBound.as_bound expected) (convert_lsb bound)))
                  [
                    (* Bounded by constants *)
                    (mk, "x <= 0");
                    (mk ~c:5 [], "x <= 5");
                    (mk ~c:3 [], "x <= 7 - 10");
                    (mk ~c:7 [], "x <= 7 + y - y");
                    (mk ~c:10 [], "x <= y && y <= 10");
                    (mk ~c:15 [], "x <= y + 7 && y <= 8");
                    (mk ~c:10 [], "x <= y && x <= 10");
                    (* Bounded by variable *)
                    (mk ["y"], "x <= y");
                    (mk ["y"], "x <= y && x >= y");
                    (mk ["y"], "x <= y && x > 5");
                    (* TODO Better heuristic for optimize vars: (AddsConstant 0 ["y"] "x <= y && y <= z"); *)
                    (mk ["z"], "x <= z && y <= z");
                    (* Bounded by constant plus variable *)
                    (mk ~c:5 ["y"], "x <= y + 5");
                    (mk ~c:5 ["y"], "x <= y - 5");
                    (mk ~c:2 ["y"], "x <= y + z - 5 && z <= 7");
                    (* With factor and positive coefficients *)
                    (mk ~s:2 ["y"], "x <= 2*y");
                    (mk ~s:2 ["y"; "z"], "x <= 2*y + 2*z");
                    (mk ~s:3 ["y";"z"], "x <= 2*y + 3*z");
                  ]
      );
    ]
