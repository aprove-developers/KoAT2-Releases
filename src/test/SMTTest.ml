open Batteries
open OUnit2
open Helper
   
module Constraints = Constraints.Make(PolyImpl.Polynomial)
module Z3Solver = SMT.MakeZ3Solver(Constraints)
module Reader = Readers.Make(ProgramImpl.StdProgram)

let print_str (str : string) = str

let suite =
  "SMT Tests" >::: [
        (*"from_constraint" >::: (
        List.map (fun (testname, expected, constr) ->
            testname >:: (fun _ -> assert_equal ~cmp:String.equal ~printer:print_str expected (Z3Solver.to_string (Reader.read_constraint constr))))
                    [
                        ("Variable Equality","Hallo", "x = y");
                        
                    ]
        );*)  
        "Satisfiable" >::: (
        List.map (fun (testname,expected, constr) ->
            testname >:: (fun _ -> assert_equal_bool expected (Z3Solver.satisfiable (Reader.read_constraint constr))))
                    [
                    ("Empty",true, "");
                    ("Constant Equality",true, "1 = 1");
                    ("Variable Equality",true, "x = x");
                    ("Different Variable Equality",true, "x = y"); 
                    ("Obvious contradiction", false, "0 = 1");
                    ("Contradiction over the integers", false, "x>x^2");
                    
                    ]
        );
        "Satisfiable_Farkas" >::: (
        List.map (fun (expected,constr,atom) ->
            constr >:: (fun _ -> assert_equal_bool expected (Z3Solver.satisfiable (Constraints.farkas_transform (Reader.read_constraint constr) (Reader.read_atom atom)))))
                    [
                    (true, "x>=0", "x>=0");
                    (false, "x>=0", "x < -10");
                    (true ,"x >= 0 && y>=0 && x+y <= 4", "2*x+y >= 0");
                    ]
        );
  ]
