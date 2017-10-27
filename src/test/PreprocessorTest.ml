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
      
    ]

    
