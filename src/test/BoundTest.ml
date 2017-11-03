open Batteries
open OUnit2
open Helper
   
let tests = 
  "Bound" >::: [
                        
      ("simplify" >:::
         List.map (fun (expected_bound, bound) ->
             bound >::
               (fun _ -> assert_equal_bound (Readers.read_bound expected_bound) (Readers.read_bound bound)))
                  [
                    ("0", "0");
                    ("-5", "-5");
                    ("8", "2^3");
                    ("1", "x^0");
                    ("1", "|x|^0");
                    ("|x|", "|x|^1");
                    ("3", "3^max{0,1}");
                    ("inf", "3*inf");
                    ("inf", "max {inf, inf}");
                    ("-inf", "max {-inf, -inf}");
                    ("inf", "min {inf, inf}");
                    ("-inf", "min {-inf, -inf}");
                    ("inf", "max {inf, 0}");
                    ("-inf", "min {-inf, 0}");
                    ("0", "max {0, 0}");
                    ("0", "min {0, 0}");
                    ("4", "max {min {3,7}, min{4,5}}");
                    ("inf", "1 + max {0, 0} + inf + 1 + max {0, 0} + inf");
                    ("0", "max{0, 0}+max{0, 0}");
                  ]
      );
      
    ]

