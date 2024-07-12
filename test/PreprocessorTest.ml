open Koat2
open Batteries
open OUnit2
open Helper
open! ProgramModules

let tests =
  "Preprocessors"
  >::: [
         "CutUnreachableLocations"
         >::: List.map
                (fun (expected_program, program) ->
                  program >:: fun _ ->
                  let result =
                    MaybeChanged.unpack
                      (CutUnreachableLocations.transform_program (Readers.read_program_simple program))
                  in
                  assert_equal_program (Readers.read_program_simple expected_program) result)
                [
                  ("l1(x) -> l2(x)", "l1(x) -> l2(x), l3(x) -> l4(x)");
                  ("l1(x) -> l2(x), l2(x) -> l3(x)", "l1(x) -> l2(x), l2(x) -> l3(x), l4(x) -> l5(x)");
                  ("l1(x) -> l2(x)", "l1(x) -> l2(x), l3(x) -> l3(x)");
                  ("l1(x) -> l2(x)", "l1(x) -> l2(x), l3(x) -> l3(x)");
                  ("l1(x) -> l2(x)", "l1(x) -> l2(x), l3(x) -> l4(x), l4(x) -> l5(x)");
                ];
         "CutUnsatisfiableTransitions"
         >::: List.map
                (fun (expected_program, program) ->
                  program >:: fun _ ->
                  let result =
                    MaybeChanged.unpack
                      (CutUnsatisfiableTransitions.transform_program (Readers.read_program_simple program))
                  in
                  assert_equal_program (Readers.read_program_simple expected_program) result)
                [
                  ( "l1(x) -> l2(x), l2(x) -> l3(x)",
                    "l1(x) -> l3(x) :|: 2 > 3, l1(x) -> l2(x), l2(x) -> l3(x)" );
                ];
         "Chaining"
         >::: List.map
                (fun (expected_program, program) ->
                  program >:: fun _ ->
                  let result =
                    MaybeChanged.unpack
                      (Preprocessor.lift_to_program Chaining.transform_graph
                         (Readers.read_program_simple program))
                  in
                  assert_equal_program (Readers.read_program_simple expected_program) result)
                [
                  ("l1(x) -> l2(x)", "l1(x) -> l2(x)");
                  ("l1(x) -{2}> l3(x), l1(x) -> l2(x)", "l1(x) -> l2(x), l2(x) -> l3(x)");
                  ( "l1(x) -> l2(x), l1(x) -{2}> l3(x), l1(x) -{3}> l4(x)",
                    "l1(x) -> l2(x), l2(x) -> l3(x), l3(x) -> l4(x)" );
                  ( "l1(x) -> l2(x), l3(x) -> l2(x), l1(x) -{2}> l3(x), l3(x) -{2}> l3(x)",
                    "l1(x) -> l2(x), l2(x) -> l3(x), l3(x) -> l2(x)" );
                  ("l1(x) -> l2(2*x), l1(x) -{2}> l3(6*x)", "l1(x) -> l2(2*x), l2(x) -> l3(3*x)");
                  ( "l1(x,y) -> l2(2*y,3), l1(x,y) -{2}> l3(6*y,15)",
                    "l1(x,y) -> l2(2*y,3), l2(x,y) -> l3(3*x,5*y)" );
                  ( "l1(x) -> l2(x) :|: x > 2, l1(x) -{2}> l3(x) :|: x > 2",
                    "l1(x) -> l2(x) :|: x > 2, l2(x) -> l3(x)" );
                  ( "l1(x,y) -> l2(y,y), l1(x,y) -{2}> l3(y,y) :|: y > 2",
                    "l1(x,y) -> l2(y,y), l2(x,y) -> l3(x,y) :|: x > 2" );
                ];
         "process_til_fixpoint"
         >::: List.map
                (fun (expected_program, program) ->
                  program >:: fun _ ->
                  let result =
                    Preprocessor.(
                      StandardProgram.process process_till_fixpoint
                        [ CutUnreachableLocations; CutUnsatisfiableTransitions ]
                        (Readers.read_program_simple program))
                  in
                  assert_equal_program (Readers.read_program_simple expected_program) result)
                [
                  ("l1(x) -> l2(x)", "l1(x) -> l2(x)");
                  ("l1(x) -> l2(x)", "l1(x) -> l2(x), l2(x) -> l3(x) :|: 2 > 3");
                ];
       ]
