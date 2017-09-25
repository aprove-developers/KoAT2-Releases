open Batteries
open ID
open OUnit2
open PolyTypes
open ConstraintTypes
open Helper
   
module Parser =
  struct
    module Reader = Readers.Make(ProgramImpl.StdProgram)

    let assert_equal_constr =     
        assert_equal ~cmp:ProgramImpl.StdProgram.Constraint_.(=~=) ~printer:ProgramImpl.StdProgram.Constraint_.to_string

    let tests =
      "Parser" >::: [
          "All together" >::: (
            let open ProgramImpl.StdProgram.Constraint_.Atom_.Polynomial_ in
            let open ProgramImpl.StdProgram.Constraint_.Infix in
            List.map (fun (testname, expected, atom) ->
                testname >:: (fun _ -> assert_equal_constr expected (Reader.read_constraint atom)))
                     [
                       ("Constants ",
                        value 42 < value 42 && value 1 >= value 0 && value 2 <= value 4 && value 6 <= value 7 && value 7 <= value 6,
                        " 42 < 42 && 1 >= 0 && 2 <= 4 && 6 = 7");
                       ("Constants and Variables",
                        var "x" > value 0 && var "y" < value 3,
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
  
module Methods (P : PolyTypes.Polynomial) =
  struct
    module Reader = Readers.Make(Program.Make(P))

    module C = Constraints.Make(P)                  
    module Atom = C.Atom_
    module Polynomial = Atom.Polynomial_
    module ParameterPolynomial = Polynomials.Make(Polynomial.Var)(Polynomial)
    module ParameterAtom = Atoms.Make(ParameterPolynomial)
                     
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

                     (*
    let evaluate str =
         str
      |> Reader.read_constraint
      |> fun constr -> C.models constr example_valuation
                      *)


    let assert_equal_constr =     
        assert_equal ~cmp:C.(=~=) ~printer:C.to_string
        
    let of_int = Polynomial.Value.of_int
    
    let rec list_equality (xs : Polynomial.Value.t list ) (ys : Polynomial.Value.t list) =
        match (xs, ys) with
            |([],[]) -> true
            |(x::tailxs, y::tailys) -> Polynomial.Value.(x =~= y) && (list_equality tailxs tailys)
            | (_,_) -> false
            
    let rec list_print (xs : Polynomial.Value.t list ) = "[" ^ String.concat "," (List.map Polynomial.Value.to_string xs) ^ "]"
    
    let list_list_equality xs ys = list_equality (List.concat xs) (List.concat ys)
    
    let list_list_print xs =
        xs
      |>(List.map list_print)
      |>(String.concat ",")
      
    let print_str (str : string) = str

    let tests = 
        
        "Constraints" >:::[
         (*let open ProgramImpl.StdProgram.Constraint_.Atom_.Polynomial_ in
            let open ProgramImpl.StdProgram.Constraint_ in*)

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
            
(*            ("models" >:::
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
                        ]);*)
                       
            ("drop_nonlinear" >:::
                List.map (fun (expected, constr) ->
                      constr >:: (fun _ -> assert_equal_constr (Reader.read_constraint expected) (C.drop_nonlinear (Reader.read_constraint constr) )))
                        [
                            ("","x^2 < x*y + 3");
                            ("3 < x","3 < x + y^3 - y*y^2");
                            ("x <= y && y <= z","x <= y && y <= z");

                        ]);
                        
            ("get_coefficient_vector" >:::
                List.map (fun (expected, var, constr) ->
                    constr >:: (fun _ -> assert_equal ~cmp:list_equality ~printer:list_print
                                                      (List.map Polynomial.Value.of_int expected)
                                                      (C.get_coefficient_vector (Polynomial.Var.of_string var) (Reader.read_constraint constr) )))
                        [
                            ([1; 2; 3], "x", "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                            ([1; 3; -4], "y", "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                            ([0; 0; 0], "z", "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                            ([3; 1; 7; -7], "x", "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1");
                            ([2; 3; 0; 0], "y", "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1");
                            ([4; 0; 3; -3], "z", "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1");
                        ]);
                        
            ("get_constant_vector" >:::
                List.map (fun (expected, constr) ->
                    constr >:: (fun _ -> assert_equal ~cmp:list_equality ~printer:list_print
                                                      (List.map Polynomial.Value.of_int expected)
                                                      (C.get_constant_vector (Reader.read_constraint constr))))
                        [
                            ([5; -2; 0], "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                            ([8; -4; 1; -1], "3*x + 2 * y + 4 * z <= 8 && (-1) * x - 3*y > 3 && 7 * x + 3 * z = 1");
                            ([0],"2 *x + y <= 0");
                        ]);
                        
            ("get_matrix" >:::
                List.map (fun (expected, vars, constr) ->
                    constr >:: (fun _ -> assert_equal ~cmp:list_list_equality ~printer:list_list_print
                                                      (List.map (List.map Polynomial.Value.of_int) expected)
                                                      (C.get_matrix ((Set.map Polynomial.Var.of_string (Set.of_list vars))) (Reader.read_constraint constr) )))
                        [
                            ([[1; 2; 3];[1; 3; -4]],["x";"y"], "x+y <= 5 && 2*x + 3*y <= -2 && 3*x-4*y <= 0");
                            ([[1; -1];[-1; 1]],["x";"y"], "x = y");
                            ([[-1; 0; 0; -1];[0; -1; 1; -1];[1; 1; -1; -1]],["x";"y";"z"],"x > z && z = y && x + y + z > 3");
                            ([[2];[1]],["x";"y"],"2 *x + y <= 0");
                            ([[1; 1; -1; 0];[1; 0; 0; -1]],["x";"y"],"x + y <= 4 && x <= 3 && x >= 0 && y>=0");
                        ]);
                        
            ("farkas_transform" >:::
                let open ProgramImpl.StdProgram.Constraint_.Atom_.Polynomial_ in
                let open ProgramImpl.StdProgram.Constraint_ in
                let open ProgramImpl.StdProgram.Constraint_.Infix in
                    let assert_equal_constr = assert_equal ~cmp:ProgramImpl.StdProgram.Constraint_.(=~=) ~printer:ProgramImpl.StdProgram.Constraint_.to_string in
                List.map (fun (expected, constr, atom) ->
                      (ProgramImpl.StdProgram.Constraint_.Atom_.to_string atom) >:: (fun _ -> assert_equal_constr expected (farkas_transform constr atom )))
                        [
                          ( (all [ (((value 1)*(helper 1)) + ((value 1)*(helper 2)) + ((value (-1))*(helper 3))) = value 2;
                                   (((value 1)*(helper 1)) + ((value (-1))*(helper 4))) = value 1;
                                   helper 1 >= value 0;
                                   helper 2 >= value 0;
                                   helper 3 >= value 0;
                                   helper 4 >= value 0;
                                   (value 4)*(helper 1) + (value 3) * (helper 2) <= value 0
                            ]), 
                            (all [(var "x")+(var "y") <= value 4;
                                  var "x" <= value 3;
                                  var "x" >= value 0;
                                  var "y" >= value 0
                            ]),
                            ProgramImpl.StdProgram.Constraint_.Atom_.Infix.(((value 2) * (var "x")) + (var "y") <= value 0));
                            
                            (all ([mk_eq ((value (-1))*(helper 1))(value (-1));mk_ge (helper 1) (value 0);mk_le (value 0) (value 0)]), 
                            (all [mk_ge (var "x") (value 0)]),
                            ProgramImpl.StdProgram.Constraint_.Atom_.mk_ge (var "x") (value 0)); 
                        ]);
        ]

      end
