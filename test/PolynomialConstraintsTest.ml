open Batteries
open ID
open Z3
open OUnit2
open PolyTypes
open ConstraintTypes
   
module PolynomialConstraintsParserTest(C : ParseablePolynomialConstraintsAtom) =
  struct
    module Parser = PolynomialConstraintsAtomParser.Make(C)
    module Lexer = PolynomialConstraintsAtomLexer.Make(C)

    let to_atom_and_back str =
         str
      |> Lexing.from_string
      |> Parser.polynomialConstraintAtom Lexer.read
      |> C.to_string

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

module PolynomialConstraintsTest (C : PolynomialConstraintsAtom) =
  struct
    module Parser = PolynomialConstraintsAtomParser.Make(C)
    module Lexer = PolynomialConstraintsAtomLexer.Make(C)
                     
    let to_atom str =
         str
      |> Lexing.from_string
      |> Parser.polynomialConstraintAtom Lexer.read

    let to_atom_and_back str =
         str
      |> to_atom
      |> C.to_string

    let example_valuation = C.Polynomial_.Valuation_.from [(C.Var.of_string "x", C.Value.of_int 3);
                                                           (C.Var.of_string "y", C.Value.of_int 5);
                                                           (C.Var.of_string "z", C.Value.of_int 7)]
                                        
    let example_renaming = C.Polynomial_.RenameMap_.from [(C.Var.of_string "x"), (C.Var.of_string "a");
                                                          (C.Var.of_string "y"), (C.Var.of_string "b");
                                                          (C.Var.of_string "z"), (C.Var.of_string "c")]
    
    
    let varlist_to_string varl =
        varl
        |> List.map C.Var.to_string
        |> String.concat ","
                                    
    let rename str =
         str
      |> to_atom
      |> fun atom -> C.rename_vars atom example_renaming
      
    let evaluate str =
         str
      |> to_atom
      |> fun atom -> C.eval_bool atom example_valuation
      
    let assert_equal_string =
        assert_equal ~cmp:String.equal

    let assert_true = assert_bool ""
    let assert_false b = assert_true (not b)
    
    
    let rec equal_varlist varl1 varl2 = 
        let sort1 = (List.sort C.Var.compare varl1) in
        let sort2 = (List.sort C.Var.compare varl2) in
        match (sort1,sort2) with 
        | ([],[]) -> true
        | (h1::t1, h2::t2) -> (C.Var.(==) h1 h2) && (equal_varlist t1 t2)
        | (_,_) -> false
        
    let assert_equal_varlist = 
        
        assert_equal ~cmp:equal_varlist

    let tests = 
        "ConstraintsAtom" >:::[
            ("is_lt" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_lt (to_atom atom))))
                        [
                            (true, " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                            (false,  " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");
                        ]);
            
            ("is_le" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_le (to_atom atom))))
                        [
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                            (true,  " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");                                   

                        ]);
            
            ("is_gt" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_gt (to_atom atom))))
                        [
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                            (false,  " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                            (true, " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");
                        ]);
                        
            ("is_ge" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_ge (to_atom atom))))
                        [
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                            (false,  " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                            (true, " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");
                        ]);
            ("is_eq" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_eq (to_atom atom))))
                        [
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                            (false,  " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                            (true, " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");
                        ]);
            ("is_neq" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_neq (to_atom atom))))
                        [
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                            (false,  " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z ");
                            (true, " x^5+y^6-z^3 <> x^2+ 5*x*y*z ");
                        ]);
                        
            ("(==)" >:::
                List.map (fun (expected, atom1, atom2) ->
                      atom1 >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.(==) (to_atom atom1) (to_atom atom2))))
                        [
                            (true, " x^5+y^6-z^3 < x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <= x^2+ 5*x*y*z ", "a*b*c + 2*z^3 +7*y^17 < a*b*c + 2*z^3 +7*y^17");
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z ", "a*b*c + 2*z^3 +7*y^17 < a*b*c + 2*z^3 +7*y^17");
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z ", "a*b*c + 2*z^3 +7*y^17 < a*b*c + 2*z^3 +7*y^17");
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z ", "a*b*c + 2*z^3 +7*y^17 < a*b*c + 2*z^3 +7*y^17");
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z ", "a*b*c + 2*z^3 +7*y^17 < a*b*c + 2*z^3 +7*y^17");
                        ]);
                        
            ("is_same_constr" >:::
                List.map (fun (expected, atom1, atom2) ->
                      atom1 >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_same_constr (to_atom atom1) (to_atom atom2))))
                        [
                            (true, " x^5+y^6-z^3 < x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 < x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 > x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 >= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 == x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 < x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <> x^2+ 5*x*y*z " );
                            
                            (false, " x^5+y^6-z^3 <= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 < x^2+ 5*x*y*z " );
                            (true, " x^5+y^6-z^3 <= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 > x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 >= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 == x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <> x^2+ 5*x*y*z " );
                            
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 < x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <= x^2+ 5*x*y*z " );
                            (true, " x^5+y^6-z^3 > x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 > x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 >= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 == x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 > x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <> x^2+ 5*x*y*z " );
                            
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 < x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 > x^2+ 5*x*y*z " );
                            (true, " x^5+y^6-z^3 >= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 >= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 == x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 >= x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <> x^2+ 5*x*y*z " );
                            
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 < x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 > x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 >= x^2+ 5*x*y*z " );
                            (true, " x^5+y^6-z^3 == x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 == x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 == x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <> x^2+ 5*x*y*z " );
                            
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 < x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 > x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 >= x^2+ 5*x*y*z " );
                            (false, " x^5+y^6-z^3 <> x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 == x^2+ 5*x*y*z " );
                            (true, " x^5+y^6-z^3 <> x^2+ 5*x*y*z "," x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c -7*y^17 <> x^2+ 5*x*y*z " );
                            

                        ]);
            ("get_variables" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal_varlist ~printer:varlist_to_string expected (C.get_variables (to_atom atom) )))
                        [
                            ([C.Var.of_string "x"], " x^3+2*x -1 < x^5 " );
                            ([C.Var.of_string "x"; C.Var.of_string "y"; C.Var.of_string "z"], " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z " );

                        ]);
                        
            ("rename_vars" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal ~cmp:C.(==) ~printer:C.to_string (to_atom expected) (rename atom )))
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
                      atom1 >:: (fun _ -> assert_equal ~printer:Bool.to_string expected (C.is_redundant (to_atom atom1) (to_atom atom2))))
                        [
                            (true, "x < y", "y > x");
                            (true, "x <= a^2 + b * 3 -6", "x <= a^2 + b * 3 -6" );
                            (false, "x <= a^2 + b * 3 -6" , "x >= a^2 + b * 3 -6" );

                        ]);
                        

        ]
        
      end  
      
module StringIDPolynomialConstraintsTest = PolynomialConstraintsTest(PolynomialConstraintsAtoms.MakePolynomialConstraintsAtom(StringID)(Number.MakeNumeric(Big_int)))
module MockPolynomialConstraintParserTest = PolynomialConstraintsParserTest(Mocks.PolynomialConstraintAtom)
                                         
let suite =
  "Suite" >::: [
      MockPolynomialConstraintParserTest.tests;
      StringIDPolynomialConstraintsTest.tests;
    ]
                     
let () =
  run_test_tt_main suite

    
