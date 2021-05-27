open Batteries
open OUnit2
open Helper
open TWN
open ProgramTypes

let tests =
  "TWN" >::: [
      ("check_twn" >:::
         List.map (fun (expected_bool, program) ->
             program >:: (fun _ ->
                     let result = TWN.check_twn(TransitionSet.any
                                                 (TransitionSet.filter (fun (l,_,l') -> Location.equal l l')
                                                 (Program.transitions (Readers.read_program_simple program)))) in
                      Printf.printf "Program: %S \n" (Program.to_string (Readers.read_program_simple program));
                     assert_equal_bool expected_bool result))
                  [
                    (true, "l0 -> l1(x,y,z), l1 -> l1(x + y + z, y + z, z)");
                    (false, "l0 -> l1(x,y,z), l1 -> l1(x + y + z, y + z, z + y)");
                    (false, "l0 -> l1(x,y,z), l1 -> l1(x + y + z, y + x, z)");
                    (false, "l0 -> l1(x,y,z,u,v,w), l1 -> l1(x + y, y + z, z + u, u + v, v + w, w + x)");
                    (true, "l0 -> l1(x,y,z,u,v,w), l1 -> l1(x + y, y + z, z + u, u + v, v + w, w)");
                  ]
      );
  ]