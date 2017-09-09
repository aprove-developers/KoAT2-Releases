open Batteries
open OUnit2
open Helper
   
module Z3Solver = SMT.MakeZ3Solver(Constraints.Make(PolyImpl.Polynomial))
module Reader = Readers.Make(ProgramImpl.StdProgram)

let suite =
  "Satisfiable" >::: (
    List.map (fun (testname, constr) ->
        testname >:: (fun _ -> assert_true (Z3Solver.satisfiable (Reader.read_constraint constr))))
             [
               ("Empty", "");
               ("Constant Equality", "1 = 1");
               ("Variable Equality", "x = x");
               (* Should work? ("Different Variable Equality", "x = y"); *)
             ]
  )
