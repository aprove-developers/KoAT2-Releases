open Koat2
open Batteries
open OUnit2
open Helper
open ProgramModules

module Check_Solvable = Check_Solvable.Make(ProgramModules)


let to_string arg =
  if Option.is_some arg then
    "solvable: " ^ (Util.enum_to_string (Util.enum_to_string Var.to_string) (Option.get arg |> List.map List.enum |> List.enum))
  else
    "not solvable"

let tests =
  "Solvable" >::: [
      ("check_solvable" >:::
         List.map (fun (expected_string, program) ->
             program >:: (fun _ ->
                     let result = Check_Solvable.check_solvable_ (Readers.read_program_simple program |> Program.sccs |> List.of_enum |> List.first |> TransitionSet.any) in
                     assert_equal_string expected_string (to_string result)))
                  [
                    ("solvable: [[Arg_0]; [Arg_1]; [Arg_2]; [Arg_3]; [Arg_4]; [Arg_5]; [Arg_6]; [Arg_7]]", "l0 -> l1(x), l1 -> l1(x)");
                    ("not solvable", "l0 -> l1(x), l1 -> l1(x^2)");
                    ("not solvable", "l0 -> l1(x,y), l1 -> l1(y^2,x^2)");
                    ("solvable: [[Arg_1; Arg_0]; [Arg_2]; [Arg_3]; [Arg_4]; [Arg_5]; [Arg_6]; [Arg_7]]", "l0 -> l1(x,y), l1 -> l1(x + y,x + y)");
                    ("solvable: [[Arg_1]; [Arg_0]; [Arg_2]; [Arg_3]; [Arg_4]; [Arg_5]; [Arg_6]; [Arg_7]]", "l0 -> l1(x,y), l1 -> l1(x + y^2,y)"); (* Note that there might be multiple ways to define the blocks of a solvable loop. *)
                    ("not solvable", "l0 -> l1(x,y), l1 -> l1(x + y + y^2,x + y)");
                    ("solvable: [[Arg_0]; [Arg_1]; [Arg_2]; [Arg_3]; [Arg_4]; [Arg_5]; [Arg_6]; [Arg_7]]", "l0 -> l1(x,y,z), l1 -> l1(x + 5,y + x^2, z + y^2)");
                    ("solvable: [[Arg_0]; [Arg_1]; [Arg_2]; [Arg_3]; [Arg_4]; [Arg_5]; [Arg_6]; [Arg_7]]", "l0 -> l1(x,y,z), l1 -> l1(x + 5,y + x^2, z + y^2)");
                    ("not solvable", "l0 -> l1(x,y,z), l1 -> l1(x + 5,y + x^2 + z, z + y^2)");
                    ("not solvable", "l0 -> l1(x,y,z,q), l1 -> l1(x + 5 + y,y + x + z^2, z + y, z)");
                    ("solvable: [[Arg_2; Arg_1; Arg_0]; [Arg_7]; [Arg_3]; [Arg_4]; [Arg_5]; [Arg_6]]", "l0 -> l1(x,y,z,q), l1 -> l1(x + 5 + y,y + x + z, z + y, q + z)");
                    ("solvable: [[Arg_2; Arg_1; Arg_0]; [Arg_7]; [Arg_3]; [Arg_4]; [Arg_5]; [Arg_6]]", "l0 -> l1(x,y,z,q), l1 -> l1(x + 5 + y,y + x + z, z + y, q + z*z)");
                  ]
      );
  ]

