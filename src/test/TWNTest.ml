open Batteries
open OUnit2
open Helper
open ProgramTypes
open PolyExponential
open Polynomials
open BoundsInst
   
let tests = 
  "TWN" >::: [ 
    ("check_twn" >:::
         List.map (fun (expected_bool, program) ->
             program >:: (fun _ -> 
                     let result = TWN.check_twn((Readers.read_program_simple program) |> Program.sccs |> List.of_enum |> List.first |> TransitionSet.any) in
                     assert_equal_bool expected_bool result))
                  [
                    (true, "l0 -> l1(x,y,z), l1 -> l1(x + y + z, y + z, z)");
                    (false, "l0 -> l1(x,y,z), l1 -> l1(x + y + z, y + z, z + y)");
                    (false, "l0 -> l1(x,y,z), l1 -> l1(x + y + z, y + x, z)");
                    (false, "l0 -> l1(x,y,z,u,v,w), l1 -> l1(x + y, y + z, z + u, u + v, v + w, w + x)");
                    (true, "l0 -> l1(x,y,z,u,v,w), l1 -> l1(x + y, y + z, z + u, u + v, v + w, w)");
                  ]
      );

      ("check_closed-form" >:::
         List.map (fun (expected_string, expr) ->
             "" >:: (fun _ -> 
                     let input = List.map (fun (str1, str2) -> (Var.of_string str1, Readers.read_polynomial str2)) expr in
                     let result = Util.enum_to_string  PE.to_string ((PE.compute_closed_form input) |> List.enum) in
                     assert_equal_string expected_string result))
                  [
                    ("[[[n == 0]] * x + [[n != 0]] * 42]", [("x","42")]);
                    ("[[[n == 0]] * x + [[n != 0]] * 42; [[n == 0]] * y + [[n != 0]] * 17]", [("x","42"); ("y","17")]);
                    ("[[[n == 0]] * x + [[n != 0]] * 42; [[n == 0]] * y + [[n != 0, n == 1]] * x + [[n != 0, n != 1]] * 42]", [("x","42"); ("y","x")]);
                    ("[[[n == 0]] * x + [[n != 0]] * 42; [[n == 0]] * y + [[n != 0, n == 1]] * x^2 + [[n != 0, n != 1]] * 1764]", [("x","42"); ("y","x*x")]);
                    ("[[[n == 0]] * x + [[n != 0]] * 42; [[n == 0]] * y + [[n != 0, n == 1]] * x^2 + [[n != 0, n != 1]] * 1764; [[n == 0]] * z + [[n != 0, n == 1]] * (x+xy+5*y) + [[n != 0, n != 1]] * 42 + [[n != 0, n != 1, n == 2]] * 47*x^2 + [[n != 0, n != 1, n != 2]] * 82908]", [("x","42"); ("y","x*x"); ("z", "x+x*y+5*y")]);
                    ("[x]", [("x","x")]);
                    ("[x * 5^n]", [("x","5*x")]);
                    ("[x; y + [[n != 0]] * x * n^1]", [("x","x"); ("y", "y + x")]);
                    ("[x; y + [[n != 0]] * x^3 * n^1]", [("x","x"); ("y", "y + x * x * x")]);
                    ("[x; y + [[n != 0]] * 42*x^3 * n^1]", [("x","x"); ("y", "y + 42 * x * x * x")]);
                    ("[x; y * 2^n + [[n != 0]] * x * 2^n + [[n != 0]] * -x]", [("x","x"); ("y", "2*y + x")]);
                    ("[x + [[n != 0]] * 2 * n^1; [[n == 0]] * z + [[n != 0]] * x+1 + [[n != 0, n != 1]] * 2 * n^1 + [[n != 0, n != 1]] * -2]", [("x", "x + 2"); ("z", "x+1")]); 
                    ("[[[n == 0]] * w + [[n != 0]] * 2; y * 4^n + [[n != 0]] * 1/2*w-2/3 * 4^n + [[n != 0]] * 2/3 + [[n != 0, n != 1]] * 1/3 * 4^n + [[n != 0, n != 1]] * -4/3]", [("w","2"); ("y", "2*w+4*y-2")]);
                    ("[x; y + [[n != 0]] * -2*x^2 * n^1; z + [[n != 0]] * xy^2 * n^1 + [[n != 0, n != 1]] * 4/3*x^5 * n^3 + [[n != 0, n != 1]] * (-2*x^5-2*x^3y) * n^2 + [[n != 0, n != 1]] * (2/3*x^5+2*x^3y) * n^1]", [("x", "x"); ("y", "y - 2 * x * x"); ("z", "z + y * y * x")]);
                    ("[y + [[n != 0]] * n^1; x + [[n != 0]] * y * n^1 + [[n != 0, n != 1]] * 1/2 * n^2 + [[n != 0, n != 1]] * -1/2 * n^1]", [("y", "y + 1"); ("x", "x + y")]);
                    ("[x * 2^n; y * 3^n + [[n != 0]] * x * 3^n + [[n != 0]] * -x * 2^n]", [("x","2*x"); ("y","3*y + x")]);
                  ]
      ); 

      ("check_normalized_closed-form" >:::
         List.map (fun (expected_string, expr) ->
             "" >:: (fun _ -> 
                     let input = List.map (fun (str1, str2) -> (Var.of_string str1, Readers.read_polynomial str2)) expr in
                     let result = Util.enum_to_string  PE.to_string ((PE.compute_closed_form input) |> PE.normalize |> List.enum) in
                     assert_equal_string expected_string result))
                  [
                    ("[42]", [("x","42")]);
                    ("[42; 17]", [("x","42"); ("y","17")]);
                    ("[42; 42]", [("x","42"); ("y","x")]);
                    ("[42; 1764]", [("x","42"); ("y","x*x")]);
                    ("[42; 1764; 82950]", [("x","42"); ("y","x*x"); ("z", "x+x*y+5*y")]);
                    ("[x]", [("x","x")]);
                    ("[x * 5^n]", [("x","5*x")]);
                    ("[x; x * n^1 + y]", [("x","x"); ("y", "y + x")]);
                    ("[x; x^3 * n^1 + y]", [("x","x"); ("y", "y + x * x * x")]);
                    ("[x; 42*x^3 * n^1 + y]", [("x","x"); ("y", "y + 42 * x * x * x")]);
                    ("[x; (y+x) * 2^n + -x]", [("x","x"); ("y", "2*y + x")]);
                    ("[2 * n^1 + x; 2 * n^1 + x-1]", [("x", "x + 2"); ("z", "x+1")]); 
                    ("[2; (y+1/2*w-1/3) * 4^n + -2/3]", [("w","2"); ("y", "2*w+4*y-2")]);
                    ("[x; -2*x^2 * n^1 + y; 4/3*x^5 * n^3 + (-2*x^5-2*x^3y) * n^2 + (xy^2+2/3*x^5+2*x^3y) * n^1 + z]", [("x", "x"); ("y", "y - 2 * x * x"); ("z", "z + y * y * x")]);
                    ("[n^1 + y; 1/2 * n^2 + y-1/2 * n^1 + x]", [("y", "y + 1"); ("x", "x + y")]);

                  ]
      );

      ("check_termination" >:::
         List.map (fun (expected_bool, program) ->
             program >:: (fun _ -> 
                     let result = TWN.termination((Readers.read_program_simple program) |> Program.sccs |> List.of_enum |> List.first |> TransitionSet.any |> Tuple3.second) in
                     assert_equal_bool expected_bool result))
                  [
                    (false, "l0 -> l1(x), l1 -> l1(x)");
                    (false, "l0 -> l1(x), l1 -> l1(x) :|: x > 0");
                    (false, "l0 -> l1(x,y), l1 -> l1(x,x*x) :|: y <= 0");
                    (true, "l0 -> l1(x,y), l1 -> l1(x,x*x) :|: y <= 0 && y != 0");
                    (true, "l0 -> l1(x,y), l1 -> l1(x - y * y,y + 1) :|: x >= 0");
                    (true, "l0 -> l1(x), l1 -> l1(42) :|: x <= 0");
                    (true, "l0 -> l1(x,y), l1 -> l1(42,26) :|: x <= 10 && y + x <= 68");
                    (false, "l0 -> l1(x,y), l1 -> l1(42,26) :|: x <= 42 && y + x <= 68");
                    (true, "l0 -> l1(x,y), l1 -> l1(42,26) :|: x <= 42 && y + x <= 67");
                    (false, "l0 -> l1(x,y,z), l1 -> l1(x + y*z*z, y, z-2*y*y) :|: x + y*y > 0"); 
                    (true, "l0 -> l1(x,y,z), l1 -> l1(x, y-2*x*x, z + x*x*y*y) :|: x + y*y < 0");
                    (false, "l0 -> l1(x,y,z), l1 -> l1(x + y*y*z*z, y, z-2*y*y) :|: x + y*y < 0");
                    (true, "l0 -> l1(x,y,z), l1 -> l1(x + y*y*z*z + 1, y, z-2*y*y) :|: x + y*y < 0");
                    (true, "l0 -> l1(x,y,z), l1 -> l1(x + y*y*z*z, y, z-2*y*y) :|: x + y*y < 0 && y != 0 && z != 0");
                    (true, "l0 -> l1(x,y), l1 -> l1(x + y,y + 1) :|: x < 0");
                  ]
      );

      ("OurInt.is_ge" >:::
         List.map (fun (expected_bool, a, b) ->
             "" >:: (fun _ -> 
                     let result = OurInt.is_ge (OurInt.of_int a) (OurInt.of_int b) in
                     assert_equal_bool expected_bool result))
                  [
                    (true, 5, 2);
                    (true, 5, 5);
                    (true, -1, -1);
                    (false, -5, 30);
                  ]
      );

      ("TWN.monotonicity_th" >:::
         List.map (fun (expected_int, (b1, a1), (b2, a2), k) ->
             "" >:: (fun _ -> 
                     let result = TWN.monotonicity_th (OurInt.of_int b1, OurInt.of_int a1) (OurInt.of_int b2, OurInt.of_int a2) (OurInt.of_int k) |> OurInt.to_int in
                     assert_equal_int expected_int result))
                  [
                    (15, (1,2), (1,1), 14);
                    (1, (1,1), (1,0), 0);
                    (1, (7,0), (5,0), 1);
                    (1, (7,2), (5,1), 1);
                    (5, (7,2), (5,1), 20);
                    (18, (7,2), (5,3), 20);
                    (29, (7,2), (5,4), 20);
                    (4, (22,5), (15,4), 12);
                    (0, (3,0), (2,1), 1);
                  ]
      );

      ("complexity" >:::
         List.map (fun (expected_string, program) ->
             "" >:: (fun _ -> 
                     let result = TWN.complexity((Readers.read_program_simple program) |> Program.sccs |> List.of_enum |> List.first |> TransitionSet.any |> Tuple3.second) in
                     assert_equal_string expected_string (Bound.to_string result)))
                  [
                     (* ("8*x+13 {O(n)}", "l0 -> l1(x,y), l1 -> l1(x + y,y + 1) :|: x < 0"); *)
                   (* ("2 {O(1)}", "l0 -> l1(x,y), l1 -> l1(x,x*x) :|: y < 0");
                    ("2 {O(1)}", "l0 -> l1(x), l1 -> l1(42) :|: x <= 0");
                    ("2 {O(1)}", "l0 -> l1(x,y), l1 -> l1(42,26) :|: x <= 42 && y + x <= 67"); *)
                    (* ("4+4+2*x+2*x", "l0 -> l1(x,y), l1 -> l1(2*x,3*y) :|: x >= y && y >= 1"); *)
                    (* ("inf {Infinity}", "l0 -> l1(x,y), l1 -> l1(x*x,3*y) :|: x >= y && y >= 1");
                    ("inf {Infinity}", "l0 -> l1(x,y), l1 -> l1(x*x,3*y) :|: x >= y && y >= 1");
                    ("inf {Infinity}", "l0 -> l1(x,y), l1 -> l1(x,3*y+x*y) :|: x >= y && y >= 1"); *)
                    (* ("", "l0 -> l1(x,y,z), l1 -> l1(x + y*y*z*z + 1, y, z-2*y*y) :|: x + y*y < 0"); *)
                  ]
      );
      
  ] 