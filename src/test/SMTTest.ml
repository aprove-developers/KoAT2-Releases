open Batteries
open OUnit2
open Helper
open Formulas
open Constraints
open Polynomials

module Z3Solver = SMT.Z3Solver

module Valuation = Valuation.Make(OurInt)

let print_fl = Float.to_string

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
            testname >:: (fun _ -> assert_equal_bool expected (Z3Solver.satisfiable (Readers.read_formula constr))))
                    [
                        ("Empty",true, "");
                        ("Constant Equality",true, "1 = 1");
                        ("Variable Equality",true, "x = x");
                        ("Different Variable Equality",true, "x = y");
                        ("Obvious contradiction", false, "0 = 1");
                        ("Sofisticated contradiction" , false, "x > y && y> z && z>x");
                        (* Not solvable by smt ("Contradiction over the integers", false, "x > x^2"); *)
                        ("Example from linear programming_1",true,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0 && 2660 * x + 1700 * y >= 54239 ");
                        ("Example from linear programming_2",false,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0 && 2660 * x + 1700 * y > 54240 ");

                    ]
        );
        (*"Satisfiable_Farkas" >::: (
        List.map (fun (expected,constr,atom) ->
            constr >:: (fun _ -> assert_equal_bool expected (Z3Solver.satisfiable (Formula.mk (Constraint.farkas_transform (Readers.read_constraint constr) (Readers.read_atom atom))))))
                    [
                        (true, "x>=0", "x>=0");
                        (false, "x>=0", "x < -10");
                        (true ,"x >= 0 && y>=0 && x+y <= 4", "2*x+y >= 0");
                        (false,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0","2660 * x + 1700 * y < 54240 ");
                        (true,"x+y<=24 && x + y <= 25 && x<= 14 && y <= 20 && x >= 0 && y>= 0","2660 * x + 1700 * y <= 54240 ");
                    ]
        );*)

        "get_model_valuation" >::: (
        List.map (fun (testname, expected, constr, poly) ->
            testname >:: (fun _ -> assert_equal_poly
                                     (Readers.read_polynomial expected)
                                     (Polynomial.eval_partial (Readers.read_polynomial poly) (Z3Solver.get_model (Readers.read_formula constr) |? (Valuation.from [])) )))
                    [
                        ("fst", "x+y","a = 1 && b = 1", "a*x+b*y" );
                        ("snd", "x+y","a = 1 && b = 1 && a = 0 || a = 1 && b = 1 && c >= 0 && c <= 0 && d = 0", "a*x+b*y + c*z + d*w" );
                    ]
        );

(*        "get_model_advanced" >::: (
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
