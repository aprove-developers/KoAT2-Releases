open Batteries
open OUnit2
open Helper

let tests = 
  "Preprocessors" >::: [
      
      ("CutUnreachable" >:::
         List.map (fun (expected_program, program) ->
             program >:: (fun _ -> assert_equal_program
                                     (Readers.read_program_simple expected_program)
                                     (PreprocessorTypes.CutUnreachable.transform_program (Readers.read_program_simple program))))
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
                        |> PreprocessorTypes.TrivialTimeBounds.transform
                        |> Tuple2.second
                        |> (fun time -> Approximation.timebound_between time (Program.Location.of_string src) (Program.Location.of_string target))
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

    ]

    
