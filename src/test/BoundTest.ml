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
                    ("inf", "max {inf, inf}");
                    ("neg inf", "max {neg inf, neg inf}");
                    ("inf", "min {inf, inf}");
                    ("neg inf", "min {neg inf, neg inf}");
                    ("inf", "max {inf, 0}");
                    ("neg inf", "min {neg inf, 0}");
                  ]
      );
      
    ]

