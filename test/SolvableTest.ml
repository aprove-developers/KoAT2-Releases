open Koat2
open OurBase
open OUnit2
open Helper
open ProgramModules
module Check_Solvable = Check_Solvable.Make (Bounds.Bound) (ProgramModules)
module Loop = Loop.Make (Bounds.Bound) (ProgramModules)

let to_string arg =
  if Option.is_some arg then
    "solvable: "
    ^ Util.sequence_to_string
        ~f:(Util.sequence_to_string ~f:Var.to_string)
        (Option.value_exn arg |> List.map ~f:Sequence.of_list |> Sequence.of_list)
  else
    "not solvable"


let tests =
  "Solvable"
  >::: [
         "check_solvable"
         >::: List.map
                ~f:(fun (expected_string, program) ->
                  program >:: fun _ ->
                  let result =
                    Check_Solvable.check_solvable_
                      (Readers.read_program_simple program |> Program.sccs |> List.hd_exn
                     |> Base.Set.choose_exn)
                  in
                  assert_equal_string expected_string (to_string result))
                [
                  ("solvable: [[Arg_0]]", "l0(x) -> l1(x), l1(x) -> l1(x)");
                  ("not solvable", "l0(x) -> l1(x), l1(x) -> l1(x^2)");
                  ("not solvable", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(y^2,x^2)");
                  ("solvable: [[Arg_1; Arg_0]]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x + y,x + y)");
                  ("solvable: [[Arg_1]; [Arg_0]]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x + y^2,y)");
                  (* Note that there might be multiple ways to define the blocks of a solvable loop. *)
                  ("not solvable", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x + y + y^2,x + y)");
                  ( "solvable: [[Arg_0]; [Arg_1]; [Arg_2]]",
                    "l0(x,y,z) -> l1(x,y,z), l1(x,y,z) -> l1(x + 5,y + x^2, z + y^2)" );
                  ( "solvable: [[Arg_0]; [Arg_1]; [Arg_2]]",
                    "l0(x,y,z) -> l1(x,y,z), l1(x,y,z) -> l1(x + 5,y + x^2, z + y^2)" );
                  ("not solvable", "l0(x,y,z) -> l1(x,y,z), l1(x,y,z) -> l1(x + 5,y + x^2 + z, z + y^2)");
                  ( "not solvable",
                    "l0(x,y,z,q) -> l1(x,y,z,q), l1(x,y,z,q) -> l1(x + 5 + y,y + x + z^2, z + y, z)" );
                  ( "solvable: [[Arg_2; Arg_1; Arg_0]; [Arg_3]]",
                    "l0(x,y,z,q) -> l1(x,y,z,q), l1(x,y,z,q) -> l1(x + 5 + y,y + x + z, z + y, q + z)" );
                  ( "solvable: [[Arg_2; Arg_1; Arg_0]; [Arg_3]]",
                    "l0(x,y,z,q) -> l1(x,y,z,q), l1(x,y,z,q) -> l1(x + 5 + y,y + x + z, z + y, q + z*z)" );
                ];
         "closed_form"
         >::: List.map
                ~f:(fun (expected_str, program) ->
                  program >:: fun _ ->
                  let loop =
                    Readers.read_program_simple program |> Program.sccs |> List.hd_exn |> Set.choose_exn
                    |> Tuple3.second |> Loop.mk
                  in
                  let closed_form = Option.value_exn @@ Check_Solvable.compute_closed_form loop in
                  let res =
                    List.map
                      (Set.to_list @@ Loop.vars loop)
                      ~f:(fun x ->
                        Tuple2.second @@ List.find_exn closed_form ~f:(fun (y, _) -> Var.equal x y))
                    |> PolyExponential.ComplexPE.normalize |> Sequence.of_list
                    |> Util.sequence_to_string ~f:PolyExponential.ComplexPE.to_string
                  in
                  assert_equal_string expected_str res)
                [
                  ("[Arg_0]", "l0(x) -> l1(x), l1(x) -> l1(x)");
                  ("[42]", "l0(x) -> l1(x), l1(x) -> l1(42)");
                  ("[42; 17]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(42,17)");
                  ("[42; Arg_1]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(42,y)");
                  ("[42; 1764]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(42,x*x)");
                  ("[42; 1764]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(42,x*x)");
                  ( "[((1/2+-3/2i)*Arg_0-1i*Arg_1) * (1i)^n + ((1/2+3/2i)*Arg_0+1i*Arg_1) * (-1i)^n; \
                     (5/2i*Arg_0+(1/2+3/2i)*Arg_1) * (1i)^n + ((1/2+-3/2i)*Arg_1-5/2i*Arg_0) * (-1i)^n]",
                    "l0(x,y) -> l1(x,y), l1(x,y) -> l1(3*x + 2*y,-5*x-3*y)" );
                  ("[42; 1764; 82950]", "l0(x,y,z) -> l1(x,y,z), l1(x,y,z) -> l1(42,x*x,x+x*y+5*y)");
                  ("[Arg_0 * (5)^n]", "l0(x) -> l1(x), l1(x) -> l1(5*x)");
                  ("[Arg_0; Arg_0 * n^1 + Arg_1]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x,x + y)");
                  ("[Arg_0; Arg_0^3 * n^1 + Arg_1]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x,x*x*x + y)");
                  ("[Arg_0; 42*Arg_0^3 * n^1 + Arg_1]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x,y + 42*x*x*x)");
                  ("[Arg_0; (Arg_0+Arg_1) * (2)^n + -Arg_0]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x,x + 2*y)");
                  ("[2 * n^1 + Arg_0; 2 * n^1 + (Arg_0-1)]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x + 2,x + 1)");
                  ( "[2; (1/2*Arg_0+Arg_1-1/3) * (4)^n + -2/3]",
                    "l0(x,y) -> l1(x,y), l1(x,y) -> l1(2,2*x+4*y-2)" );
                  ( "[Arg_0; -2*Arg_0^2 * n^1 + Arg_1; 4/3*Arg_0^5 * n^3 + (-2*Arg_0^3*Arg_1-2*Arg_0^5) * \
                     n^2 + (Arg_0*Arg_1^2+2*Arg_0^3*Arg_1+2/3*Arg_0^5) * n^1 + Arg_2]",
                    "l0(x,y,z) -> l1(x,y,z), l1(x,y,z) -> l1(x,y - 2 * x * x,z + y * y * x)" );
                  ("[Arg_0; (1+Arg_0) * n^1 + Arg_1]", "l0(x,y) -> l1(x,y), l1(x,y) -> l1(x,x + y + 1)");
                  ( "[2*Arg_1 * n^1 + Arg_0; Arg_1; Arg_1; 2*Arg_1 * n^1 + (Arg_0-Arg_1); -1 * n^1 + Arg_4]",
                    "l0(a,b,c,d,e) -> l1(a,b,c,d,e), l1(a,b,c,d,e) -> l1(a+2*b,b,b,a+b,e-1) :|: 1 <= e" );
                ];
       ]
