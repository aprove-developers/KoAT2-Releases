open Batteries
open OUnit2
open Helper
open Formulas   
                  
let tests =      
  "Formula" >:::[
      ("mk_le_than_max" >:::
         List.map (fun (expected, lhs, rhs) ->
             (lhs ^ " < " ^ "max{" ^ (String.concat "," rhs) ^ "}") >::
               (fun _ -> assert_equal_formula (Readers.read_formula expected) (Formula.mk_le_than_max (Readers.read_polynomial lhs) (List.map Readers.read_polynomial rhs))))
                  [
                    ("0 = 1", "x", []);
                    ("x <= y", "x", ["y"]);
                    ("x <= y || x <= z", "x", ["y"; "z"]);
                  ]
      );
    ]
      
