open Koat2
open Batteries
open OUnit2
open ProgramModules
open Formulas

let tests =
  "Invariant Generation"
  >::: [
         "Simple"
         >::: List.map
                (fun (l, l', invariant, program_str) ->
                  program_str >:: fun _ ->
                  let result_program =
                    program_str |> Readers.read_program_simple |> InvariantGeneration.transform_program
                    |> MaybeChanged.unpack
                  in
                  let fulfiled =
                    result_program |> Program.graph
                    |> (fun graph ->
                         TransitionGraph.find_all_edges graph (Location.of_string l) (Location.of_string l'))
                    |> List.exists (fun (l, t, l') ->
                           SMT.Z3Solver.tautology
                             Formula.Infix.(
                               Formula.mk (TransitionLabel.guard t) => Readers.read_formula invariant))
                  in
                  assert_bool
                    (String.concat " "
                       [
                         invariant;
                         "was not generated for a transition from location";
                         l;
                         "to";
                         l';
                         "in a result program";
                         Program.to_simple_string result_program;
                       ])
                    fulfiled)
                [
                  (* Simple propagation to next transition *)
                  ("b", "c", "Arg_0 > 0", "a(x) -> b(x) :|: x > 0, b(x) -> c(x)");
                  (* More complex propagation to next transition *)
                  ( "b",
                    "c",
                    "Arg_0 > 0 && Arg_0 < Arg_1",
                    "a(x,y) -> b(x,y) :|: x > 0 && x < y, b(x,y) -> c(x,y)" );
                  (* Propagation into a constant loop *)
                  ("b", "b", "Arg_0 > 0", "a(x) -> b(x) :|: x > 0, b(x) -> b(x)");
                  (* Propagation after a constant loop *)
                  ("b", "c", "Arg_0 > 0", "a(x) -> b(x) :|: x > 0, b(x) -> b(x), b(x) -> c(x)");
                  (* Propagation after a modifying loop *)
                  ("b", "c", "Arg_0 > 0", "a(x) -> b(x) :|: x > 0, b(x) -> b(x+1), b(x) -> c(x)");
                  (* Fixed value propagation *)
                  ("b", "b", "Arg_1 = 1", " a(x,y)->b(x,1), b(x,y)->b(x-y,y) :|: x>0");
                  (* Complex program invariants from exp2.koat *)
                  ( "g",
                    "g",
                    "Arg_2 = 1",
                    "f(x, y, z)-> g(x, 1, 1),  g(x, y, z)-> g(x - 1, 2 * y, z):|: x > 0,  g(x, y, z)-> h(x, \
                     y, z):|: x <= 0,  h(x, y, z)-> h(x, y - 1, 2 * z):|: y > 0,  h(x, y, z)-> i(x, y, z):|: \
                     y <= 0, i(x, y, z)-> i(x, y, z - 1):|: z > 0" );
                  ( "h",
                    "h",
                    "Arg_0<=0",
                    "f(x, y, z)-> g(x, 1, 1),  g(x, y, z)-> g(x - 1, 2 * y, z):|: x > 0,  g(x, y, z)-> h(x, \
                     y, z):|: x <= 0,  h(x, y, z)-> h(x, y - 1, 2 * z):|: y > 0,  h(x, y, z)-> i(x, y, z):|: \
                     y <= 0, i(x, y, z)-> i(x, y, z - 1):|: z > 0" );
                  ( "i",
                    "i",
                    "Arg_0<=0 && Arg_1<=0",
                    "f(x, y, z)-> g(x, 1, 1),  g(x, y, z)-> g(x - 1, 2 * y, z):|: x > 0,  g(x, y, z)-> h(x, \
                     y, z):|: x <= 0,  h(x, y, z)-> h(x, y - 1, 2 * z):|: y > 0,  h(x, y, z)-> i(x, y, z):|: \
                     y <= 0, i(x, y, z)-> i(x, y, z - 1):|: z > 0" );
                ];
       ]
