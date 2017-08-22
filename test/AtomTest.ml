open Batteries
open ID
open OUnit2
open PolyTypes
open ConstraintTypes
open Helper
   
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
                testname >:: (fun _ -> assert_equal_string expected (to_atom_and_back atom)))
                        [
                        ("Constants LT", "42 < 42", " 42 < 42 ");
                        ("Constants LE", "42 <= 42", " 42 <= 42 ");
                        ("Constants GT", "42 > 42", " 42 > 42 ");
                        ("Constants GE", "42 >= 42", " 42 >= 42 ");
                        ("Constant and Poly LT", "42 < ((x^2)+(((5*x)*y)*z))", " 42 < x^2+ 5*x*y*z ");
                        ("Constant and Poly LE", "42 <= ((x^2)+(((5*x)*y)*z))", " 42 <= x^2+ 5*x*y*z ");
                        ("Constant and Poly GT", "42 > ((x^2)+(((5*x)*y)*z))", " 42 > x^2+ 5*x*y*z ");
                        ("Constant and Poly GE", "42 >= ((x^2)+(((5*x)*y)*z))", " 42 >= x^2+ 5*x*y*z ");
                        ("Poly and Poly LT", "(((x^5)+(y^6))+(-(z^3))) < ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 < x^2+ 5*x*y*z ");
                        ("Poly and Poly LE", "(((x^5)+(y^6))+(-(z^3))) <= ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 <= x^2+ 5*x*y*z ");
                        ("Poly and Poly GT", "(((x^5)+(y^6))+(-(z^3))) > ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 > x^2+ 5*x*y*z ");
                        ("Poly and Poly GE", "(((x^5)+(y^6))+(-(z^3))) >= ((x^2)+(((5*x)*y)*z))", " x^5+y^6-z^3 >= x^2+ 5*x*y*z ");
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

    let varset_to_string varl =
        varl
        |> Set.map Polynomial.Var.to_string
        |> Set.to_list
        |> String.concat ","
                                    
    let rename str rename_map =
         str
      |> Reader.read_atom
      |> fun atom -> Atom.rename atom (Polynomial.RenameMap_.from_native rename_map)
      
    let evaluate str valuation =
         str
      |> Reader.read_atom
      |> fun atom -> Atom.models atom (Polynomial.Valuation_.from_native valuation)
      
    let assert_equal_atom =
      assert_equal ~cmp:C.Atom_.(=~=) ~printer:C.Atom_.to_string

    let tests = 
        "ConstraintsAtom" >:::[
                        
            ("(=~=)" >:::
                List.map (fun (atom1, atom2) ->
                    (atom1 ^ "=~=" ^ atom2) >:: (fun _ -> assert_equal_atom (Reader.read_atom atom1) (Reader.read_atom atom2)))
                                                  [
                                                    ("x < y", "y > x");
                                                    ("x <= y", "y >= x");
                                                    ("x <= y", "x - 1 < y");
                                                    ("x > y", "x - 1 >= y");
                                                    ("4*x > 2*y", "2*x > y");
                                                    ("x*y < x", "x * (y - 1) < 0");
                  ]);
                        
            ("vars" >:::
                List.map (fun (expected, atom) ->
                    atom >:: (fun _ -> assert_equal ~cmp:Set.equal ~printer:varset_to_string
                                                    (Set.map Polynomial.Var.of_string (Set.of_list expected))
                                                    (C.Atom_.vars (Reader.read_atom atom))))
                         [
                           (["x"], " x^3+2*x -1 < x^5 " );
                           (["x"; "y"; "z"], " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z " );
            ]);
                        
            ("rename" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal_atom (Reader.read_atom expected) (rename atom [("x", "a"); ("y", "b"); ("z", "c")])))
                        [
                            ("5 <= 5", "5 <= 5");
                            ("a <= a", "x <= x" );
                            ("a <= b", "x <= y" );
                            ("a < a ^ 2 + 2 * a * b", "x < x ^ 2 + 2 * x * y" );
                            ("a^2 * b^2 < 7", "x^2 * y^2 < 7");
            ]);
            
            ("models" >:::
                List.map (fun (expected, atom, valuation) ->
                      atom >:: (fun _ ->  assert_equal_bool expected (evaluate atom valuation)))
                        [
                            (true, " 5 <= 5 ", [("x", 3)]);
                            (true, "x < x ^ 2 + 2 * x * y", [("x", 3); ("y", 5)]);
                            (false, "x^2 * y^2 < 7", [("x", 3); ("y", 5); ("z", 7)]);
                        ]);
                        
            ("is_linear" >:::
                List.map (fun (expected, atom) ->
                      atom >:: (fun _ -> assert_equal_bool expected (C.Atom_.is_linear (Reader.read_atom atom))))
                        [
                            (true, "x < y");
                            (false, "x <= a^2 + b * 3 -6");
                            (false, "x >= a^2 + b * 3 -6" );
                        ]);                        
        ]
        
      end
