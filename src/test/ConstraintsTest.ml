open Batteries
open ID
open OUnit2
open PolyTypes
open ConstraintTypes
open Helper
   
module Parser =
  struct
    module Reader = Readers.Make(TransitionGraphImpl.StdTransitionGraph)

    (* TODO Wrong *)
    let rec equal_constr constr1 constr2 = 
        match (constr1,constr2) with 
        | ([],[]) -> true
        | (h1::t1, h2::t2) -> TransitionGraphImpl.StdTransitionGraph.Transition_.Constraint_.Atom_.(h1 =~= h2) && equal_constr t1 t2
        | (_,_) -> false

    let assert_equal_constr =     
        assert_equal ~cmp:equal_constr ~printer:TransitionGraphImpl.StdTransitionGraph.Transition_.Constraint_.to_string

    let tests =
      "Parser" >::: [
          "All together" >::: (
            let open TransitionGraphImpl.StdTransitionGraph.Transition_.Constraint_.Atom_.Polynomial_ in
            let open TransitionGraphImpl.StdTransitionGraph.Transition_.Constraint_ in
            List.map (fun (testname, expected, atom) ->
                testname >:: (fun _ -> assert_equal_constr expected (Reader.read_constraint atom)))
                     [
                       ("Constants ",
                        all [mk_lt (value 42) (value 42); mk_ge (value 1) (value 0); mk_le (value 2) (value 4); mk_le (value 6) (value 7); mk_le (value 7) (value 6)],
                        " 42 < 42 && 1 >= 0 && 2 <= 4 && 6 = 7");
                       ("Constants and Variables",
                        all [mk_gt (var "x") (value 0); mk_lt (var "y") (value 3)],
                        "x > 0 && y < 3");
                     ]
          );
          "Negative Tests" >::: (
            List.map (fun (testname, atom) ->
                testname >:: (fun _ -> assert_exception (fun _ -> Reader.read_constraint atom)))
                     [
                       ("Unexpected char: =", "x == y");
                     ]
          );
        ]
        
  end
  
module Methods (C : Constraint) =
  struct
    module Reader = Readers.Make(TransitionGraph.MakeTransitionGraph(TransitionGraph.MakeTransition(C)))

    module Atom = C.Atom_
    module Polynomial = Atom.Polynomial_
                     
    let example_valuation = Polynomial.Valuation_.from_native [("x", 3);
                                                        ("y", 5);
                                                        ("z", 7)]
                          
    let example_renaming = Polynomial.RenameMap_.from_native [("x", "a"); ("y", "b"); ("z", "c")]
    
    
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
      |> fun constr -> C.models constr example_valuation

                                   (* TODO Wrong *)
    let rec equal_constr constr1 constr2 = 
        match (constr1,constr2) with 
        | ([],[]) -> true
        | (h1::t1, h2::t2) -> C.Atom_.(h1 =~= h2) && equal_constr t1 t2
        | (_,_) -> false
            

    let assert_equal_constr =     
        assert_equal ~cmp:equal_constr ~printer:C.to_string

    let tests = 

        "Constraints" >:::[

            ("get_variables" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal ~cmp:Set.equal ~printer:varset_to_string (Set.map Polynomial.Var.of_string (Set.of_list expected)) (C.vars (Reader.read_constraint constr) )))
                        [
                            (["x"; "y"; "z"], " x^5+y^6-z^3 + a*b*c + 2*z^3 +7*y^17 - a*b*c - 2*z^3 -7*y^17 < x^2+ 5*x*y*z && x > 0 && y >= 0 && z <= 4" );

                        ]);
                        
            ("rename_vars" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_constr (Reader.read_constraint expected) (rename constr )))
                        [
                            ("5 <= 5", " 5 <= 5 " );
                            ("a <= a", "x <= x" );
                            ("a < a ^ 2 + 2 * a * b", "x < x ^ 2 + 2 * x * y" );
                            ("a^2 * b^2 < 7", "x^2 * y^2 < 7");
                            ("a <= a && b < c", "x <= x && y < z" );

                        ]);
            ("models" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ ->  assert_equal_bool expected (evaluate constr )))
                        [
                            (true, " 5 <= 5 " );
                            (true, "x <= x" );
                            (true,"x < x ^ 2 + 2 * x * y" );
                            (false, "x^5+y^6-z^3 = x * y * z + x^2 ");
                            (false , "x^2 * y^2 < 7");
                            (true , "2 < 3 && 3 < 4 && 4 < 5");
                            (false, "3 <= 3 && 2 <= 2 && 1 <= 0");
                        ]);
                        
            ("drop_nonlinear" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_constr (Reader.read_constraint expected) (C.drop_nonlinear (Reader.read_constraint constr) )))
                        [
                            ("","x^2 < x*y + 3");
                            ("3 < x","3 < x + y^3 - y*y^2");
                            ("x <= y && y <= z","x <= y && y <= z");

                        ]);

        ]
        
      end
