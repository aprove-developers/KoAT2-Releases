open Batteries
open OUnit2
open Helper
open LocalSizeBound
open Formulas
open LocalSizeBound
   
let tests = 
  "LocalSizeBound" >::: [
                        
      ("find upper bound" >:::
         List.map (fun (expected, guard) ->
             "bound for x with " ^ guard >::
               (fun _ -> find_bound `Upper (Var.of_string "x") (Readers.read_formula guard) 1024
                         |> Option.map (fun bound () -> assert_equal_lsb expected bound)
                         |? (fun () -> assert_failure "No bound")
                         |> fun f -> f () ))
                  [
                    (* Bounded by constants *)
                    (mk `Upper, "x <= 0");
                    (mk ~c:5 `Upper, "x <= 5");
                    (mk ~c:(-5) `Upper, "x <= -5");
                    (mk ~c:(-3) `Upper, "x <= 7 - 10");
                    (mk ~c:7 `Upper, "x <= 7 + y - y");
                    (mk ~c:10 `Upper, "x <= y && y <= 10");
                    (mk ~c:15 `Upper, "x <= y + 7 && y <= 8");
                    (mk ~c:10 `Upper, "x <= y && x <= 10");
                    (* Bounded by variable *)
                    (mk ~pos_pure:["y"] `Upper, "x <= y");
                    (mk ~pos_pure:["y"] `Upper, "x <= y && x >= y");
                    (mk ~pos_pure:["y"] `Upper, "x <= y && x > 5");
                    (* TODO Better heuristic for optimize vars: (AddsConstant 0 ["y"] "x <= y && y <= z"); *)
                    (mk ~pos_pure:["z"] `Upper, "x <= z && y <= z");
                    (* Bounded by constant plus variable *)
                    (mk ~c:5 ~pos_pure:["y"] `Upper, "x <= y + 5");
                    (mk ~c:(-5) ~pos_pure:["y"] `Upper, "x <= y - 5");
                    (mk ~c:2 ~pos_pure:["y"] `Upper, "x <= y + z - 5 && z <= 7");
                    (* With factor and positive coefficients *)
                    (mk ~s:2 ~pos_pure:["y"] `Upper, "x <= 2*y");
                    (mk ~s:2 ~pos_pure:["y"; "z"] `Upper, "x <= 2*y + 2*z");
                    (mk ~s:3 ~pos_abs:["y"] ~pos_pure:["z"] `Upper, "x <= 2*y + 3*z");
                    (* With factor and negative coefficients *)
                    (mk ~s:2 ~neg_pure:["y"] `Upper, "x <= (-2)*y");
                    (mk ~s:2 ~neg_pure:["y"; "z"] `Upper, "x <= (-2)*y + (-2)*z");
                    (mk ~s:3 ~neg_abs:["y"] ~neg_pure:["z"] `Upper, "x <= (-2)*y + (-3)*z");
                    (* With factor and mixed coefficients *)
                    (mk ~s:3 ~pos_abs:["y"] ~neg_pure:["z"] `Upper, "x <= 2*y + (-3)*z");
                    (mk ~s:2 ~pos_pure:["y"] `Upper, "x <= 2*y + (-3)*z && z >= 0");
                    (mk ~s:3 ~neg_pure:["z"] `Upper, "x <= 2*y + (-3)*z && y <= 0");
                    (mk ~s:2 ~neg_pure:["y"] `Upper, "x <= (-2)*y + (-3)*z && z >= 0");
                    (mk ~s:3 ~pos_pure:["z"] `Upper, "x <= 2*y + 3*z && y <= 0");
                  ]
      );

      ("find lower bound" >:::
         List.map (fun (expected, guard) ->
             "bound for x with " ^ guard >::
               (fun _ -> find_bound `Lower (Var.of_string "x") (Readers.read_formula guard) 1024
                         |> Option.map (fun bound () -> assert_equal_lsb expected bound)
                         |? (fun () -> assert_failure "No bound")
                         |> fun f -> f () ))
                  [
                    (* Bounded by constants *)
                    (mk `Lower, "x >= 0");
                    (mk ~c:5 `Lower, "x >= 5");
                    (mk ~c:(-5) `Lower, "x >= -5");
                    (mk ~c:(-3) `Lower, "x >= 7 - 10");
                    (mk ~c:7 `Lower, "x >= 7 + y - y");
                    (mk ~c:10 `Lower, "x >= y && y >= 10");
                    (mk ~c:15 `Lower, "x >= y + 7 && y >= 8");
                    (mk ~c:10 `Lower, "x >= y && x >= 10");
                    (* Bounded by variable *)
                    (mk ~pos_pure:["y"] `Lower, "x >= y");
                    (mk ~pos_pure:["y"] `Lower, "x >= y && x >= y");
                    (mk ~pos_pure:["y"] `Lower, "x >= y && x < 5");
                    (* TODO Better heuristic for optimize vars: (AddsConstant 0 ["y"] "x >= y && y >= z"); *)
                    (mk ~pos_pure:["z"] `Lower, "x >= z && y >= z");
                    (* Bounded by constant plus variable *)
                    (mk ~c:5 ~pos_pure:["y"] `Lower, "x >= y + 5");
                    (mk ~c:(-5) ~pos_pure:["y"] `Lower, "x >= y - 5");
                    (mk ~c:2 ~pos_pure:["y"] `Lower, "x >= y + z - 5 && z >= 7");
                    (* With factor and positive coefficients *)
                    (mk ~s:2 ~pos_pure:["y"] `Lower, "x >= 2*y");
                    (mk ~s:2 ~pos_pure:["y"; "z"] `Lower, "x >= 2*y + 2*z");
                    (mk ~s:3 ~pos_abs:["y"] ~pos_pure:["z"] `Lower, "x >= 2*y + 3*z");
                    (* With factor and negative coefficients *)
                    (mk ~s:2 ~neg_pure:["y"] `Lower, "x >= (-2)*y");
                    (mk ~s:2 ~neg_pure:["y"; "z"] `Lower, "x >= (-2)*y + (-2)*z");
                    (mk ~s:3 ~neg_abs:["y"] ~neg_pure:["z"] `Lower, "x >= (-2)*y + (-3)*z");
                    (* With factor and mixed coefficients *)
                    (mk ~s:3 ~pos_abs:["y"] ~neg_pure:["z"] `Lower, "x >= 2*y + (-3)*z");
                    (mk ~s:2 ~pos_pure:["y"] `Lower, "x >= 2*y + (-3)*z && z <= 0");
                    (mk ~s:3 ~neg_pure:["z"] ~pos_pure:["y"] `Lower, "x >= 2*y + (-3)*z && y <= 0");
                    (mk ~s:2 ~neg_pure:["y"] `Lower, "x >= (-2)*y + (-3)*z && z <= 0");
                    (mk ~s:3 ~pos_pure:["y"; "z"] `Lower, "x >= 2*y + 3*z && y <= 0");
                  ]
      );
    ]
