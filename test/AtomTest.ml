open Batteries
open ID
open OUnit2
open PolyTypes
open ConstraintTypes
   
module Parser =
  struct
    module Reader = Readers.Make(Mocks.TransitionGraph)

    let to_atom_and_back str =
         str
      |> Reader.read_atom
      |> Mocks.TransitionGraph.Transition_.Constraint_.Atom_.to_string

    let tests =
        "Parser" >::: [
            "Positive Tests" >::: (
                List.map (fun (testname, expected, atom) ->
                testname >:: (fun _ -> assert_equal expected (to_atom_and_back atom)))
                        [
                        ("Constants LT", "42 < 42", " 42 < 42 ");
                        ("Constants LE", "42 <= 42", " 42 <= 42 ");
                        ("Constants GT", "42 > 42", " 42 > 42 ");
                        ("Constants GE", "42 >= 42", " 42 >= 42 ");
                        ("Constants EQ", "42 == 42", " 42 == 42 ");
                        ("Constants NEQ", "42 <> 42", " 42 <> 42 ");
                        ("Constant and Poly LT", "42 < ((x^2)+(((5*x)*y)*z))", " 42 < x^2+ 5*x*y*z ");
                        ("Constant and Poly LE", "42 <= ((x^2)+(((5*x)*y)*z))", " 42 <= x^2+ 5*x*y*z ");
                        ("Constant and Poly GT", "42 > ((x^2)+(((5*x)*y)*z))", " 42 > x^2+ 5*x*y*z ");
                        ("Constant and Poly GE", "42 >= ((x^2)+(((5*x)*y)*z))", " 42 >= x^2+ 5*x*y*z ");
                        ("Constant and Poly EQ", "42 == ((x^2)+(((5*x)*y)*z))", " 42 == x^2+ 5*x*y*z ");
                        ("Constant and Poly NEQ", "42 <> ((x^2)+(((5*x)*y)*z))", " 42 <> x^2+ 5*x*y*z ");
                        ("Poly and Poly LT", "(((x^5)+(y^6))+(-(z^3))) < ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                        ("Poly and Poly LE", "(((x^5)+(y^6))+(-(z^3))) <= ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                        ("Poly and Poly GT", "(((x^5)+(y^6))+(-(z^3))) > ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                        ("Poly and Poly GE", "(((x^5)+(y^6))+(-(z^3))) >= ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                        ("Poly and Poly EQ", "(((x^5)+(y^6))+(-(z^3))) == ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                        ("Poly and Poly NEQ", "(((x^5)+(y^6))+(-(z^3))) <> ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");
                        ]
            );
            "Negative Tests" >::: (
                List.map (fun (testname, atom) ->
                    testname >:: (fun _ -> assert_raises (Reader.Lexer.SyntaxError (testname)) (fun _ -> to_atom_and_back atom)))
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

    let to_atom_and_back str =
         str
      |> Reader.read_atom
      |> C.Atom_.to_string

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
      |> Reader.read_atom
      |> fun atom -> Atom.rename atom example_renaming
      
    let evaluate str =
         str
      |> Reader.read_atom
      |> fun atom -> Atom.eval_bool atom example_valuation
      
    let assert_equal_string =
        assert_equal ~cmp:String.equal

    let assert_true = assert_bool ""
    let assert_false b = assert_true (not b)
    
    
    let tests = 
        let default_poly_l_1 = "x^5+y^6-z^3" in
        let default_poly_r_1 = "x^2+ 5*x*y*z" in
        let default_poly_l_2 = "x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17" in
                 
        "ConstraintsAtom" >:::[
            
            
            ("is_lt" >:::  
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_lt (Reader.read_atom atom))))
                      (List.map (fun (a,b)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1))
                        [
                            (true, " < ");
                            (false,  " <= ");
                            (false, " > ");
                            (false, " >= ");
                            (false, " == ");
                            (false, " <> ");
                        ])
            );
            
            ("is_le" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_le (Reader.read_atom atom))))
                        (List.map (fun (a,b)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1))
                        [
                            (false, " < ");
                            (true,  " <= ");
                            (false, " > ");
                            (false, " >= ");
                            (false, " == ");
                            (false, " <> ");
                        ])
            );
            
            ("is_gt" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_gt (Reader.read_atom atom))))
                        (List.map (fun (a,b)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1))
                        [
                            (false, " < ");
                            (false,  " <= ");
                            (true, " > ");
                            (false, " >= ");
                            (false, " == ");
                            (false, " <> ");
                        ])
            );
                        
            ("is_ge" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_ge (Reader.read_atom atom))))
                        (List.map (fun (a,b)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1))
                        [
                            (false, " < ");
                            (false,  " <= ");
                            (false, " > ");
                            (true, " >= ");
                            (false, " == ");
                            (false, " <> ");
                        ])
            );
            ("is_eq" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_eq (Reader.read_atom atom))))
                        (List.map (fun (a,b)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1))
                        [
                            (false, " < ");
                            (false,  " <= ");
                            (false, " > ");
                            (false, " >= ");
                            (true, " == ");
                            (false, " <> ");
                        ])
            );
            
            ("is_neq" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_neq (Reader.read_atom atom))))
                        (List.map (fun (a,b)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1))
                        [
                            (false, " < ");
                            (false,  " <= ");
                            (false, " > ");
                            (false, " >= ");
                            (false, " == ");
                            (true, " <> ");
                        ])
            );
                        
            ("(==)" >:::
                List.map (fun (expected, atom1, atom2) ->
                    (atom1 ^ "==" ^ atom2) >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.(==) (Reader.read_atom atom1) (Reader.read_atom atom2))))
                        (List.map (fun (a,b,c)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1, default_poly_l_2 ^ c ^ default_poly_r_1)) (List.map (fun (e,f)-> if (String.compare e f == 0) then (true, e, f) else (false, e, f)) (List.cartesian_product ["<"; "<="; ">"; ">="; "=="; "<>"] ["<"; "<="; ">"; ">="; "=="; "<>"]))) 
            );
                        
            ("is_same_constr" >:::
                List.map (fun (expected, atom1, atom2) ->
                      atom1 >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_same (Reader.read_atom atom1) (Reader.read_atom atom2))))
                        (List.map (fun (a,b,c)-> (a, default_poly_l_1 ^ b ^ default_poly_r_1, default_poly_l_2 ^ c ^ default_poly_r_1)) (List.map (fun (e,f)-> if (String.compare e f == 0) then (true, e, f) else (false, e, f)) (List.cartesian_product ["<"; "<="; ">"; ">="; "=="; "<>"] ["<"; "<="; ">"; ">="; "=="; "<>"]))) 
            );
            
            ("get_variables" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~cmp:Set.equal ~printer:varset_to_string (Set.map Polynomial.Var.of_string (Set.of_list expected)) (C.Atom_.vars (Reader.read_atom atom) )))
                        [
                            (["x"], " x^3+2*x -1 < x^5 " );
                            (["x"; "y"; "z"], " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z " );

                        ]);
                        
            ("rename_vars" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~cmp:C.Atom_.(==) ~printer:C.Atom_.to_string (Reader.read_atom expected) (rename atom )))
                        [
                            ("5 <= 5", " 5 <= 5 " );
                            ("a == a", "x == x" );
                            ("a < a ^ 2 + 2 * a * b", "x < x ^ 2 + 2 * x * y" );
                            ("a^5+b^6-c^3 <> a * b * c + a^2 " , "x^5+y^6-z^3 <> x * y * z + x^2 ");
                            ("a^2 * b^2 < 7", "x^2 * y^2 < 7");


                        ]);
            ("eval_bool" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ ->  assert_equal ~printer:Bool.to_string (expected) (evaluate atom )))
                        [
                            (true, " 5 <= 5 " );
                            (true, "x == x" );
                            (true,"x < x ^ 2 + 2 * x * y" );
                            (false, "x^5+y^6-z^3 == x * y * z + x^2 ");
                            (false , "x^2 * y^2 < 7");
                            
                        ]);
                        
            ("is_redundant" >:::
                List.map (fun (expected, atom1, atom2) ->
                      atom1 >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_redundant (Reader.read_atom atom1) (Reader.read_atom atom2))))
                        [
                            (true, "x < y", "y > x");
                            (true, "x <= a^2 + b * 3 -6", "x <= a^2 + b * 3 -6" );
                            (false, "x <= a^2 + b * 3 -6" , "x >= a^2 + b * 3 -6" );

                        ]);
            
            ("is_linear" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_linear (Reader.read_atom atom))))
                        [
                            (true, "x < y");
                            (false, "x <= a^2 + b * 3 -6");
                            (false, "x >= a^2 + b * 3 -6" );
                            (true, "x+y-2*z+3*x ^ 2 - x*x - x*x - x*x == 7 * z + 4 * y");
                            (false, "x*x + y*z <> a^17");

                        ]);

            ("normalise" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~cmp:C.Atom_.(==) ~printer:C.Atom_.to_string (Reader.read_atom expected) (C.Atom_.normalise (Reader.read_atom atom))))
                        [
                            ("x<=3","x<=3");
                            ("a + b + c <= -2", "a+2 <= -b-c");
                            ("y + z > 4","-4 + y > -z ");
                            ("a + b + c < -2", "a+2 < -b-c");
                            ("x^2 - y^3 == 8", "10 + x^2 - 1 == y^3 + 3 + 5 + 9");
                        ]);
                        

        ]
        
      end
