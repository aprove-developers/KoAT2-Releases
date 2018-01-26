open Batteries
open OUnit2
open Helper
open ProgramTypes
   
let tests = 
  "Preprocessors" >::: [
      
      ("CutUnreachableLocations" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ ->
                     let result = MaybeChanged.unpack (CutUnreachableLocations.transform_program (Readers.read_program_simple program)) in
                     reset ();
                     assert_equal_program
                                     (Readers.read_program_simple expected_program)
                                     result))
                  [
                    ("l1 -> l2(x)", "l1 -> l2(x), l3 -> l4(x)");
                    ("l1 -> l2(x), l2 -> l3(x)", "l1 -> l2(x), l2 -> l3(x), l4 -> l5(x)");
                    ("l1 -> l2(x)", "l1 -> l2(x), l3 -> l3(x)");
                    ("l1 -> l2(x)", "l1 -> l2(x), l3 -> l3(x)");
                    ("l1 -> l2(x)", "l1 -> l2(x), l3 -> l4(x), l4 -> l5(x)");
                  ]
      );
      
(* TODO TransitionIDs instead of locations
      ("TrivialTimeBounds" >:::
         List.map (fun (src, target, program) ->
             program >:: (fun _ ->
                     let result =
                       Approximation.empty 5 5
                       |> TrivialTimeBounds.compute (Readers.read_program_simple program)
                       |> (fun time -> Approximation.timebound_between time (Location.of_string src) (Location.of_string target))
                     in
                     reset ();
                     assert_equal_bound_option (Some Bound.one) result))
                  [
                    ("l1", "l2", "l1 -> l2(x)");
                    ("l2", "l3", "l1 -> l2(x), l2 -> l3(x)");
                    ("l2", "l3", "l1 -> l2(x), l2 -> l2(x), l2 -> l3(x)");
                    ("l3", "l4", "l1 -> l2(x), l2 -> l3(x), l3 -> l2(x), l3 -> l4(x)");
                    ("l3", "l4", "l1 -> l2(x), l2 -> l3(x), l3 -> l2(x), l3 -> l4(x), l4 -> l4(x), l4 -> l5(x)");
                    ("l4", "l5", "l1 -> l2(x), l2 -> l3(x), l3 -> l2(x), l3 -> l4(x), l4 -> l4(x), l4 -> l5(x)");
                  ]
      );
 *)
      
      ("CutUnsatisfiableTransitions" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ ->
                     let result =
                       MaybeChanged.unpack (CutUnsatisfiableTransitions.transform_program (Readers.read_program_simple program))
                     in
                     reset ();
                     assert_equal_program (Readers.read_program_simple expected_program) result))
                  [
                    ("l1 -> l2(x), l2 -> l3(x)", "l1 -> l3(x) :|: 2 > 3, l1 -> l2(x), l2 -> l3(x)");
                  ]
      );

      ("Chaining" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ ->
                     let result =
                       MaybeChanged.unpack (Preprocessor.lift_to_program Chaining.transform_graph (Readers.read_program_simple program))
                     in
                     reset ();
                     assert_equal_program (Readers.read_program_simple expected_program) result))
                  [
                    ("l1 -> l2(x)", "l1 -> l2(x)");
                    ("l1 -{2}> l3(x)", "l1 -> l2(x), l2 -> l3(x)");
                    ("l1 -{3}> l4(x)", "l1 -> l2(x), l2 -> l3(x), l3 -> l4(x)");
                    ("l1 -{2}> l3(x), l3 -{2}> l3(x)", "l1 -> l2(x), l2 -> l3(x), l3 -> l2(x)");
                    ("l1 -{2}> l3(6*x)", "l1 -> l2(2*x), l2 -> l3(3*x)");
                    ("l1 -{2}> l3(6*y,15)", "l1 -> l2(2*y,3), l2 -> l3(3*x,5*y)");
                    ("l1 -{2}> l3(x) :|: x > 2", "l1 -> l2(x) :|: x > 2, l2 -> l3(x)");
                    ("l1 -{2}> l3(y) :|: y > 2", "l1 -> l2(y), l2 -> l3(x) :|: x > 2");
                  ]
      );

      ("process_til_fixpoint" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ ->
                     let result =
                       Tuple2.first (Preprocessor.process_til_fixpoint
                                       Preprocessor.[CutUnreachableLocations; CutUnsatisfiableTransitions]
                                       (Readers.read_program_simple program, Approximation.empty 0 0))
                     in
                     reset ();
                     assert_equal_program (Readers.read_program_simple expected_program) result))
                  [
                    ("l1 -> l2(x)", "l1 -> l2(x)");
                    ("l1 -> l2(x)", "l1 -> l2(x), l2 -> l3(x) :|: 2 > 3");
                  ]
      );      

    ]

    
