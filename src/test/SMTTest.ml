open Batteries
open OUnit2
open Helper
   
module Z3Solver = SMT.MakeZ3Solver(PolyImpl.Polynomial)
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
            testname >:: (fun _ -> assert_equal_bool expected (Z3Solver.satisfiable (Reader.read_formula constr))))
                    [
                        ("Empty",true, "");
                        ("Constant Equality",true, "1 = 1");
                        ("Variable Equality",true, "x = x");
                        ("Different Variable Equality",true, "x = y"); 
                        ("Obvious contradiction", false, "0 = 1");
                        ("Sofisticated contradiction" , false, "x > y && y> z && z>x");
                        ("Contradiction over the integers", false, "x > x^2");
                        ("Example from linear programming_1",true,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0 && 2660 * x + 1700 * y >= 54239 ");
                        ("Example from linear programming_2",false,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0 && 2660 * x + 1700 * y > 54240 ");
                    
                    ]
        );
        "Satisfiable_Farkas" >::: (
        List.map (fun (expected,constr,atom) ->
            constr >:: (fun _ -> assert_equal_bool expected (Z3Solver.satisfiable (Z3Solver.Formula_.mk (Z3Solver.Constraint_.farkas_transform (Reader.read_constraint constr) (Reader.read_atom atom))))))
                    [
                        (true, "x>=0", "x>=0");
                        (false, "x>=0", "x < -10");
                        (true ,"x >= 0 && y>=0 && x+y <= 4", "2*x+y >= 0");
                        (false,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0","2660 * x + 1700 * y < 54240 ");
                        (true,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0","2660 * x + 1700 * y <= 54240 ");
                    ]
        );
        
(*        "get_model" >::: (
        List.map (fun (testname, expected, constr) ->
            testname >:: (fun _ -> assert_equal ~cmp:String.equal ~printer:print_str expected (Z3Solver.get_model (Reader.read_constraint constr))))
                    [
                        ("Empty","", "");
                        ("Constant Equality","", "1 = 1");
                        ("Variable Equality","", "x = x");
                        ("Different Variable Equality","", "x = y"); 
                        ("Obvious contradiction", "", "0 = 1");
                        ("Sofisticated contradiction" , "", "x > y && y > z && z > x");
                        ("Contradiction over the integers", "", "x > x^2");
                    ]
        );
        
        "get_model_advanced" >::: (
        List.map (fun (expected, constr, atom) ->
            constr >:: (fun _ -> assert_equal ~cmp:String.equal ~printer:print_str expected (Z3Solver.get_model (Constraints.farkas_transform (Reader.read_constraint constr)(Reader.read_atom atom)))))
                    [
                        ("", "x>=0", "x>=0");
                        ("", "x>=0", "x < -10");
                        ("" ,"x >= 0 && y>=0 && x+y <= 4", "2*x+y >= 0");
                        ("","x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0","2660 * x + 1700 * y <=54240 ");
                    ]
        );*)
  ]
