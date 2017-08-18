open Batteries
open ID
open OUnit2
open PolyTypes
open ConstraintTypes
   
module Parser =
  struct
    module Reader = Readers.Make(Mocks.TransitionGraph)

    let to_constr_and_back str =
         str
      |> Reader.read_constraint
      |> Mocks.TransitionGraph.Transition_.Constraint_.to_string

    let comp_tests comp = 
      comp >::: [
          "Constants" >:: (fun _ -> assert_equal ~printer:identity
                                      ("42 " ^ comp ^ " 42")
                                      (to_constr_and_back ("42 " ^ comp ^ " 42"))
                          );
          "Constants and Poly" >:: (fun _ -> assert_equal ~printer:identity
                                               ("42 " ^ comp ^ " ((x^2)+(((5*x)*y)*z))")
                                               (to_constr_and_back (" 42 " ^ comp ^ " x^2+ 5*x*y*z "))
                                   );
          "Poly and Poly" >:: (fun _ -> assert_equal ~printer:identity
                                          ("(((x^5)+(y^6))+(-(z^3))) " ^ comp ^ " ((x^2)+(((5*x)*y)*z))")
                                          (to_constr_and_back (" x^5+y^6-z^3 " ^ comp ^ " x^2+ 5*x*y*z "))
                              );
        ]
      
    let tests =
      "Parser" >::: [
          "All comparators" >::: List.map comp_tests ["<"; "<="; ">"; ">="; "=="; "<>"];
          "All together" >::: (
            List.map (fun (testname, expected, atom) ->
                testname >:: (fun _ -> assert_equal ~printer:identity expected (to_constr_and_back atom)))
                     [
                       ("Constants ", "42 < 42 /\ 1 >= 0 /\ 2 <= 4 /\ 6 == 7", " 42 < 42 && 1 >= 0 && 2 <= 4 && 6 == 7");
                       ("Constants and Variables", "x > 0 /\ y < 3", "x > 0 && y < 3");
                       ("Constants and Polynomials", "(x*x) > 0 /\ (y^2) < 3 /\ ((x^2)+(((5*x)*y)*z)) <> 17", "x*x > 0 && y^2 < 3 && x^2+ 5*x*y*z <> 17");
                       ("Polynomial Constraints", "(((x^5)+(y^6))+(-(z^3))) <> ((x^2)+(((5*x)*y)*z)) /\ (z^3) < (x+y)","x^5+y^6-z^3 <> x^2+ 5*x*y*z && z^3 < x +y" );
                     ]
          );
          "Negative Tests" >::: (
            List.map (fun (testname, atom) ->
                testname >:: (fun _ -> assert_raises (Reader.Lexer.SyntaxError (testname)) (fun _ -> to_constr_and_back atom)))
                     [
                       ("Unexpected char: =", "x = y");
                     ]
          );
        ]
        
  end
  
module Methods (C : Constraint) =
  struct
    module Reader = Readers.Make(TransitionGraph.MakeTransitionGraph(TransitionGraph.MakeTransition(C)))

    module Atom = C.Atom_
    module Polynomial = Atom.Polynomial_
                     
    let to_constr_and_back str =
         str
      |> Reader.read_constraint
      |> C.to_string

    let example_valuation = Polynomial.Valuation_.from [(Polynomial.Var.of_string "x", Polynomial.Value.of_int 3);
                                                        (Polynomial.Var.of_string "y", Polynomial.Value.of_int 5);
                                                        (Polynomial.Var.of_string "z", Polynomial.Value.of_int 7)]
                          
    let example_renaming = Polynomial.RenameMap_.from [(Polynomial.Var.of_string "x"), (Polynomial.Var.of_string "a");
                                                       (Polynomial.Var.of_string "y"), (Polynomial.Var.of_string "b");
                                                       (Polynomial.Var.of_string "z"), (Polynomial.Var.of_string "c")]
    
    
    let varset_to_string varl =
        varl
        |> Set.map Polynomial.Var.to_string
        |> Set.to_list
        |> String.concat ","
                                    
    let rename str =
         str
      |> Reader.read_constraint
      |> fun constr -> C.rename constr example_renaming
      
    let evaluate str =
         str
      |> Reader.read_constraint
      |> fun constr -> C.eval_bool constr example_valuation
      
    let assert_equal_string =
        assert_equal ~cmp:String.equal

    let assert_true = assert_bool ""
    let assert_false b = assert_true (not b)
    
    let rec equal_constr constr1 constr2 = 
        match (constr1,constr2) with 
        | ([],[]) -> true
        | (h1::t1, h2::t2) -> (C.Atom_.(==) h1 h2) && (equal_constr t1 t2)
        | (_,_) -> false
            
    let assert_equal_constr = 
    
        assert_equal ~cmp:equal_constr ~printer:C.to_string   
    
    
    let tests = 

        "Constraints" >:::[

            ("get_variables" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal ~cmp:Set.equal ~printer:varset_to_string (Set.map Polynomial.Var.of_string (Set.of_list expected)) (C.vars (Reader.read_constraint constr) )))
                        [
                            (["x"], " x^3+2*x -1 < x^5 && x <> 0 && 3 > 2 " );
                            (["x"; "y"; "z"], " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z && x > 0 && y >= 0 && z <= 4" );

                        ]);
                        
            ("rename_vars" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_constr (Reader.read_constraint expected) (rename constr )))
                        [
                            ("5 <= 5", " 5 <= 5 " );
                            ("a == a", "x == x" );
                            ("a < a ^ 2 + 2 * a * b", "x < x ^ 2 + 2 * x * y" );
                            ("a^5+b^6-c^3 <> a * b * c + a^2 " , "x^5+y^6-z^3 <> x * y * z + x^2 ");
                            ("a^2 * b^2 < 7", "x^2 * y^2 < 7");
                            ("a == a && b < c", "x == x && y < z" );
                            (" a^2 > 0 && b^2-2*c^3 <> a^5 && 2 < 5 && 2 *a < a + b + c  ", "x^2 > 0 && y^2 -2*z^3 <> x^5 && 2 < 5 && 2*x < x+y+z ")

                        ]);
            ("eval_bool" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ ->  assert_equal ~printer:Bool.to_string (expected) (evaluate constr )))
                        [
                            (true, " 5 <= 5 " );
                            (true, "x == x" );
                            (true,"x < x ^ 2 + 2 * x * y" );
                            (false, "x^5+y^6-z^3 == x * y * z + x^2 ");
                            (false , "x^2 * y^2 < 7");
                            (true , "2 < 3 && 3 < 4 && 4 < 5");
                            (false, "3 == 3 && 2 == 2 && 1 == 0");
                            (false, "x^2 > 0 && y^3 < 100 && x^5+y^6-z^3 <> x^2+ 5*x*y*z");
                            (true, "x^2 > 0 && y^3 < 126 && x^5+y^6-z^3 <> x^2+ 5*x*y*z");
                        ]);
                        
            ("drop_nonlinear" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_constr (Reader.read_constraint expected) (C.drop_nonlinear (Reader.read_constraint constr) )))
                        [
                            ("x <> 0 && 3 > 2", " x^3+2*x -1 < x^5 && x <> 0 && 3 > 2 " );
                            ("","x^2 < x*y + 3");
                            ("3 < x","3 < x + y^3 - y*y^2");
                            ("x == y && y == z","x == y && y == z");

                        ]);

            ("drop_not_equal" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_constr (Reader.read_constraint expected) (C.drop_not_equal (Reader.read_constraint constr))))
                        [
                            ("x == y ","x == y && z <> 3");
                        ]);

            ("to_less_equal" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal_constr (Reader.read_constraint expected) (C.to_less_equal (Reader.read_atom atom) )))
                        [
                            ("x <= y && y<=x ","x == y");
                            ("x <= y - 1", "x < y");
                            ("x <= y - 1","y > x");

                        ]);
        ]
        
      end
