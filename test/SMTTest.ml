open Batteries
open OUnit2
open SMT

let assert_true = assert_bool ""

let suite =
  "Satisfiable" >::: (
    List.map (fun (testname, constraints) ->
        testname >:: (fun _ -> assert_true (Z3Solver.satisfiable constraints)))
             [
               ("Empty", []);
             ]
  )
                     
let () =
  run_test_tt_main suite

