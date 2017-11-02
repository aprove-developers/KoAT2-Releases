open Batteries
open OUnit2
open Helper
open Program.Types
   
let tests = 
  "Preprocessors" >::: [
      
      ("CutUnreachableLocations" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ -> assert_equal_program
                                     (Readers.read_program_simple expected_program)
                                     (MaybeChanged.unpack (CutUnreachableLocations.transform_program (Readers.read_program_simple program)))))
                  [
                    ("l1 -> l2(x)", "l1 -> l2(x), l3 -> l4(x)");
                    ("l1 -> l2(x), l2 -> l3(x)", "l1 -> l2(x), l2 -> l3(x), l4 -> l5(x)");
                    ("l1 -> l2(x)", "l1 -> l2(x), l3 -> l3(x)");
                    ("l1 -> l1(x)", "l1 -> l1(x), l2 -> l2(x)");
                    ("l1 -> l2(x)", "l1 -> l2(x), l3 -> l4(x), l4 -> l5(x)");
                  ]
      );
      
      ("TrivialTimeBounds" >:::
         List.map (fun (src, target, program) ->
             program >:: (fun _ ->
                     assert_equal_bound_option
                       (Some Bound.one)
                       ((Readers.read_program_simple program, Approximation.empty 5 5)
                        |> TrivialTimeBounds.transform
                        |> MaybeChanged.unpack
                        |> Tuple2.second
                        |> (fun time -> Approximation.timebound_between time (Location.of_string src) (Location.of_string target))
           )))
                  [
                    ("l1", "l2", "l1 -> l2(x)");
                    ("l2", "l3", "l1 -> l2(x), l2 -> l3(x)");
                    ("l1", "l2", "l1 -> l1(x), l1 -> l2(x)");
                    ("l2", "l3", "l1 -> l2(x), l2 -> l1(x), l2 -> l3(x)");
                    ("l2", "l3", "l1 -> l2(x), l2 -> l1(x), l2 -> l3(x), l3 -> l3(x), l3 -> l4(x)");
                    ("l3", "l4", "l1 -> l2(x), l2 -> l1(x), l2 -> l3(x), l3 -> l3(x), l3 -> l4(x)");
                  ]
      );

      ("CutUnsatisfiableTransitions" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ -> assert_equal_program
                                     (Readers.read_program_simple expected_program)
                                     (MaybeChanged.unpack (CutUnsatisfiableTransitions.transform_program (Readers.read_program_simple program)))))
                  [
                    ("l1 -> l2(x), l2 -> l3(x)", "l1 -> l3(x) :|: 2 > 3, l1 -> l2(x), l2 -> l3(x)");
                  ]
      );

      ("Chaining" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ -> assert_equal_program
                                     (Readers.read_program_simple expected_program)
                                     (MaybeChanged.unpack (Preprocessor.lift_to_program Chaining.transform_graph (Readers.read_program_simple program)))))
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
             program >:: (fun _ -> assert_equal_program
                                     (Readers.read_program_simple expected_program)
                                     (Tuple2.first (Preprocessor.process_til_fixpoint
                                                      Preprocessor.[CutUnreachableLocations; CutUnsatisfiableTransitions; Chaining]
                                                      (Readers.read_program_simple program, Approximation.empty 0 0)))))
                  [
                    ("l1 -> l2(x)", "l1 -> l2(x)");
                    ("l1 -> l2(x)", "l1 -> l2(x), l2 -> l3(x) :|: 2 > 3");
                    ("l1 -> l1(x)", "l1 -> l1(x), l1 -> l2(x) :|: x > 0, l2 -> l3(x) :|: x < 0");
                  ]
      );      

    ]

    
