open Batteries
open OUnit2

module Z3Solver = SMT.MakeZ3Solver(Constraints.MakeConstraint(StdPoly.Polynomial))
module Reader = Readers.MakeReader(Constraints.MakeConstraint(StdPoly.Polynomial))

let assert_true = assert_bool ""

let suite =
  "Satisfiable" >::: (
    List.map (fun (testname, constr) ->
        testname >:: (fun _ -> assert_true (Z3Solver.satisfiable (Reader.read_constraint constr))))
             [
               ("Empty", "");
               ("Constant Equality", "1 == 1");
               ("Variable Equality", "x == x");
               (* Should work? ("Different Variable Equality", "x == y"); *)
             ]
  )
                     
let () =
  run_test_tt_main suite

