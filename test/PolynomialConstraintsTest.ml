open Batteries
open ID
open OUnit2
open PolyTypes
open ConstraintTypes
   
module PolynomialConstraintsAtomParserTest(C : ParseableConstraint) =
  struct
    module Parser = PolynomialConstraintsParser.Make(C)
    module Lexer = PolynomialConstraintsLexer.Make(C)

    let to_atom_and_back str =
         str
      |> Lexing.from_string
      |> Parser.polynomialConstraintAtom Lexer.read
      |> C.Atom_.to_string

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
                    testname >:: (fun _ -> assert_raises (Lexer.SyntaxError (testname)) (fun _ -> to_atom_and_back atom)))
                            [
                            ("Unexpected char: =", "x = y");
                            ]
                );
        ]
  end

module PolynomialConstraintsAtomTest (C : Constraint) =
  struct
    module Parser = PolynomialConstraintsParser.Make(C)
    module Lexer = PolynomialConstraintsLexer.Make(C)

    module Atom = C.Atom_
    module Polynomial = Atom.Polynomial_
                     
    let to_atom str =
         str
      |> Lexing.from_string
      |> Parser.polynomialConstraintAtom Lexer.read

    let to_atom_and_back str =
         str
      |> to_atom
      |> C.Atom_.to_string

    let example_valuation = Polynomial.Valuation_.from [(Polynomial.Var.of_string "x", Polynomial.Value.of_int 3);
                                                        (Polynomial.Var.of_string "y", Polynomial.Value.of_int 5);
                                                        (Polynomial.Var.of_string "z", Polynomial.Value.of_int 7)]
                                        
    let example_renaming = Polynomial.RenameMap_.from [(Polynomial.Var.of_string "x"), (Polynomial.Var.of_string "a");
                                                       (Polynomial.Var.of_string "y"), (Polynomial.Var.of_string "b");
                                                       (Polynomial.Var.of_string "z"), (Polynomial.Var.of_string "c")]
    
    
    let varlist_to_string varl =
        varl
        |> List.map Polynomial.Var.to_string
        |> String.concat ","
                                    
    let rename str =
         str
      |> to_atom
      |> fun atom -> Atom.rename atom example_renaming
      
    let evaluate str =
         str
      |> to_atom
      |> fun atom -> Atom.eval_bool atom example_valuation
      
    let assert_equal_string =
        assert_equal ~cmp:String.equal

    let assert_true = assert_bool ""
    let assert_false b = assert_true (not b)
    
    
    let rec equal_varlist varl1 varl2 = 
        let sort1 = (List.sort Polynomial.Var.compare varl1) in
        let sort2 = (List.sort Polynomial.Var.compare varl2) in
        match (sort1,sort2) with 
        | ([],[]) -> true
        | (h1::t1, h2::t2) -> (Polynomial.Var.(==) h1 h2) && (equal_varlist t1 t2)
        | (_,_) -> false
        
    let assert_equal_varlist = 
        
        assert_equal ~cmp:equal_varlist

    let tests = 
        let default_poly_l_1 = "x^5+y^6-z^3" in
        let default_poly_r_1 = "x^2+ 5*x*y*z" in
        let default_poly_l_2 = "x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17" in
                 
        "ConstraintsAtom" >:::[
            
            
            ("is_lt" >:::  
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_lt (to_atom atom))))
                      (List.map (fun (a,b)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1])))
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
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_le (to_atom atom))))
                        (List.map (fun (a,b)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1])))
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
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_gt (to_atom atom))))
                        (List.map (fun (a,b)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1])))
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
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_ge (to_atom atom))))
                        (List.map (fun (a,b)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1])))
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
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_eq (to_atom atom))))
                        (List.map (fun (a,b)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1])))
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
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_neq (to_atom atom))))
                        (List.map (fun (a,b)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1])))
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
                      (String.concat "==" [atom1 ; atom2]) >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.(==) (to_atom atom1) (to_atom atom2))))
                        (List.map (fun (a,b,c)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1]), (String.concat c [default_poly_l_2 ; default_poly_r_1]))) (List.map (fun (e,f)-> if (String.compare e f == 0) then (true, e, f) else (false, e, f)) (List.cartesian_product ["<"; "<="; ">"; ">="; "=="; "<>"] ["<"; "<="; ">"; ">="; "=="; "<>"]))) 
            );
                        
            ("is_same_constr" >:::
                List.map (fun (expected, atom1, atom2) ->
                      atom1 >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_same (to_atom atom1) (to_atom atom2))))
                        (List.map (fun (a,b,c)-> (a, (String.concat b [default_poly_l_1 ; default_poly_r_1]), (String.concat c [default_poly_l_2 ; default_poly_r_1]))) (List.map (fun (e,f)-> if (String.compare e f == 0) then (true, e, f) else (false, e, f)) (List.cartesian_product ["<"; "<="; ">"; ">="; "=="; "<>"] ["<"; "<="; ">"; ">="; "=="; "<>"]))) 
            );
            
            ("get_variables" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal_varlist ~printer:varlist_to_string expected (C.Atom_.vars (to_atom atom) )))
                        [
                            ([Polynomial.Var.of_string "x"], " x^3+2*x -1 < x^5 " );
                            ([Polynomial.Var.of_string "x"; Polynomial.Var.of_string "y"; Polynomial.Var.of_string "z"], " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z " );

                        ]);
                        
            ("rename_vars" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~cmp:C.Atom_.(==) ~printer:C.Atom_.to_string (to_atom expected) (rename atom )))
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
                      atom1 >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.Atom_.is_redundant (to_atom atom1) (to_atom atom2))))
                        [
                            (true, "x < y", "y > x");
                            (true, "x <= a^2 + b * 3 -6", "x <= a^2 + b * 3 -6" );
                            (false, "x <= a^2 + b * 3 -6" , "x >= a^2 + b * 3 -6" );

                        ]);
                        

        ]
        
      end
      
module PolynomialConstraintsParserTest(C : ParseableConstraint) =
  struct
    module Parser = PolynomialConstraintsParser.Make(C)
    module Lexer = PolynomialConstraintsLexer.Make(C)

    let to_constr_and_back str =
         str
      |> Lexing.from_string
      |> Parser.polynomialConstraints Lexer.read
      |> C.to_string

    
    let tests =
        "Parser" >::: [
            "Positive Tests" >::: (
                List.map (fun (testname, expected, atom) ->
                testname >:: (fun _ -> assert_equal expected (to_constr_and_back atom)))
                        [
                        ("Constants LT", "42 < 42", " 42 < 42 ");
                        ("Constants LE", "42 <= 42", " 42 <= 42 ");
                        ("Constants GT", "42 > 42", " 42 > 42 ");
                        ("Constants GE", "42 >= 42", " 42 >= 42 ");
                        ("Constants EQ", "42 == 42", " 42 == 42 ");
                        ("Constants NEQ", "42 <> 42", " 42 <> 42 ");
                        ("Constants ", "42 < 42 /\ 1 >= 0 /\ 2 <= 4 /\ 6 == 7", " 42 < 42 && 1 >= 0 && 2 <= 4 && 6 == 7");
                        
                        ("Constant and Poly LT", "42 < ((x^2)+(((5*x)*y)*z))", " 42 < x^2+ 5*x*y*z ");
                        ("Constant and Poly LE", "42 <= ((x^2)+(((5*x)*y)*z))", " 42 <= x^2+ 5*x*y*z ");
                        ("Constant and Poly GT", "42 > ((x^2)+(((5*x)*y)*z))", " 42 > x^2+ 5*x*y*z ");
                        ("Constant and Poly GE", "42 >= ((x^2)+(((5*x)*y)*z))", " 42 >= x^2+ 5*x*y*z ");
                        ("Constant and Poly EQ", "42 == ((x^2)+(((5*x)*y)*z))", " 42 == x^2+ 5*x*y*z ");
                        ("Constant and Poly NEQ", "42 <> ((x^2)+(((5*x)*y)*z))", " 42 <> x^2+ 5*x*y*z ");
                        ("Constants and Variables", "x > 0 /\ y < 3", "x > 0 && y < 3");
                        ("Constants and Polynomials", "(x*x) > 0 /\ (y^2) < 3 /\ ((x^2)+(((5*x)*y)*z)) <> 17", "x*x > 0 && y^2 < 3 && x^2+ 5*x*y*z <> 17");
                        
                        
                        ("Poly and Poly LT", "(((x^5)+(y^6))+(-(z^3))) < ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                        ("Poly and Poly LE", "(((x^5)+(y^6))+(-(z^3))) <= ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                        ("Poly and Poly GT", "(((x^5)+(y^6))+(-(z^3))) > ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                        ("Poly and Poly GE", "(((x^5)+(y^6))+(-(z^3))) >= ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                        ("Poly and Poly EQ", "(((x^5)+(y^6))+(-(z^3))) == ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                        ("Poly and Poly NEQ", "(((x^5)+(y^6))+(-(z^3))) <> ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");
                        ("Polynomial Constraints", "(((x^5)+(y^6))+(-(z^3))) <> ((x^2)+(((5*x)*y)*z)) /\ (z^3) < (x+y)","x^5+y^6-z^3 <> x^2+ 5*x*y*z && z^3 < x +y" )

                        ]
            );
            "Negative Tests" >::: 
            (
                List.map (fun (testname, atom) ->
                    testname >:: (fun _ -> assert_raises (Lexer.SyntaxError (testname)) (fun _ -> to_constr_and_back atom)))
                            [
                            ("Unexpected char: =", "x = y");
                            ]
                );
                
            
        ]
        
  end
  
module PolynomialConstraintsTest (C : Constraint) =
  struct
    module Parser = PolynomialConstraintsParser.Make(C)
    module Lexer = PolynomialConstraintsLexer.Make(C)
                     
    module Atom = C.Atom_
    module Polynomial = Atom.Polynomial_
                     
    let to_constr str =
         str
      |> Lexing.from_string
      |> Parser.polynomialConstraints Lexer.read

    let to_constr_and_back str =
         str
      |> to_constr
      |> C.to_string

    let example_valuation = Polynomial.Valuation_.from [(Polynomial.Var.of_string "x", Polynomial.Value.of_int 3);
                                                        (Polynomial.Var.of_string "y", Polynomial.Value.of_int 5);
                                                        (Polynomial.Var.of_string "z", Polynomial.Value.of_int 7)]
                          
    let example_renaming = Polynomial.RenameMap_.from [(Polynomial.Var.of_string "x"), (Polynomial.Var.of_string "a");
                                                       (Polynomial.Var.of_string "y"), (Polynomial.Var.of_string "b");
                                                       (Polynomial.Var.of_string "z"), (Polynomial.Var.of_string "c")]
    
    
    let varlist_to_string varl =
        varl
        |> List.map Polynomial.Var.to_string
        |> String.concat ","
                                    
    let rename str =
         str
      |> to_constr
      |> fun constr -> C.rename constr example_renaming
      
    let evaluate str =
         str
      |> to_constr
      |> fun constr -> C.eval_bool constr example_valuation
      
    let assert_equal_string =
        assert_equal ~cmp:String.equal

    let assert_true = assert_bool ""
    let assert_false b = assert_true (not b)
    
    
    let rec equal_varlist varl1 varl2 = 
        let sort1 = (List.sort Polynomial.Var.compare varl1) in
        let sort2 = (List.sort Polynomial.Var.compare varl2) in
        match (sort1,sort2) with 
        | ([],[]) -> true
        | (h1::t1, h2::t2) -> (Polynomial.Var.(==) h1 h2) && (equal_varlist t1 t2)
        | (_,_) -> false
        
    let assert_equal_varlist = 
        
        assert_equal ~cmp:equal_varlist
        
    let rec equal_constr constr1 constr2 = 
        match (constr1,constr2) with 
        | ([],[]) -> true
        | (h1::t1, h2::t2) -> (C.Atom_.(==) h1 h2) && (equal_constr t1 t2)
        | (_,_) -> false
            
    let assert_equal_constr = 
    
        assert_equal ~cmp:equal_constr ~printer:C.to_string
        
    let tests = 
        (*let default_poly_l_1 = "x^5+y^6-z^3" in
        let default_poly_r_1 = "x^2+ 5*x*y*z" in
        let default_poly_l_2 = "x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17" in*)

        "Constraints" >:::[

            ("get_variables" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_varlist ~printer:varlist_to_string expected (C.vars (to_constr constr) )))
                        [
                            ([Polynomial.Var.of_string "x"], " x^3+2*x -1 < x^5 && x <> 0 && 3 > 2 " );
                            ([Polynomial.Var.of_string "x"; Polynomial.Var.of_string "y"; Polynomial.Var.of_string "z"], " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z && x > 0 && y >= 0 && z <= 4" );

                        ]);
                        
            ("rename_vars" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_constr (to_constr expected) (rename constr )))
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
        ]
        
      end
      
module StringIDPolynomialConstraintsAtomTest = PolynomialConstraintsAtomTest(Constraints.MakeConstraint(Polynomials.MakePolynomial(StringID)(Number.MakeNumeric(Big_int))))
module StringIDPolynomialConstraintsTest = PolynomialConstraintsTest(Constraints.MakeConstraint(Polynomials.MakePolynomial(StringID) (Number.MakeNumeric(Big_int))))
module MockPolynomialConstraintAtomParserTest = PolynomialConstraintsAtomParserTest(Mocks.Constraint)
module MockPolynomialConstraintParserTest = PolynomialConstraintsParserTest(Mocks.Constraint)                                        

let suite =
  "Suite" >::: [
      MockPolynomialConstraintParserTest.tests;
      StringIDPolynomialConstraintsAtomTest.tests;
      MockPolynomialConstraintParserTest.tests;
      StringIDPolynomialConstraintsTest.tests
    ]
                     
let () =  run_test_tt_main suite

    
