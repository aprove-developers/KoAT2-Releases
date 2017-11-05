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

                    (* Inf *)
                    ("inf", "inf");

                    (* Const *)
                    ("0", "0");
                    ("12", "12");

                    (* Var *)
                    ("x", "x");
                    ("y", "y");

                    (* Abs *)
                    ("|x|", "|x|");
                    ("|y|", "|y|");
                    ("|5|", "|5|");
                    ("7", "|-7|");

                    (* Negation *)
                    ("-inf", "-inf");
                    ("-5", "-5");
                    ("-|x|", "-|x|");
                    ("|x|", "--|x|");

                    (* Pow v^n *)
                    ("1", "x^0");
                    ("x", "x^1");
                    ("x^2", "x^2");
                    ("8", "2^3");

                    (* Pow n^b *)
                    ("0", "0^1");
                    ("1", "2^0");
                    ("1", "1^1");
                    ("2", "2^1");
                    ("2^x", "2^x");
                    ("inf", "2^inf");
                    ("0", "2^(-inf)");
                    ("2^(x+y)", "2^(x+y)");
                    ("2^(|x|+y)", "2^(|x|+y)");
                    ("3", "3^max{0,1}");

                    (* Sum *)
                    ("x", "0+x");                   
                    ("11", "7+4");                   
                    ("2*|x|", "|x|+|x|");                   
                    ("inf", "7+inf");                   
                    ("inf", "inf+8");                   
                    ("inf", "inf+inf");                   
                    ("-inf", "-inf+8");                   
                    (* Not supported ("3+2*x", "3+x+x"); *)

                    (* Minus *)
                    ("-x", "0-x");                   
                    ("3", "7-4");                   
                    ("0", "|x|-|x|");                   
                    ("-inf", "7-inf");                   
                    ("inf", "inf-8");                   
                    ("0", "inf-inf");                   
                    ("-inf", "-inf-8");                   
                    (* Not supported ("3-2*x", "3-x-x"); *)
                    
                    (* Product *)
                    ("0", "0*x");
                    ("0", "|x|*0");
                    ("x", "1*x");
                    ("|x|", "|x|*1");
                    ("-x", "-1*x");
                    ("-|x|", "|x|*-1");
                    ("-inf", "inf*-1");
                    ("inf", "3*inf");
                    ("0", "0*inf");
                    ("-inf", "4*-inf");
                    ("-inf", "-4*inf");
                    ("inf", "inf*inf");
                    ("-inf", "-inf*inf");
                    ("-inf", "inf*-inf");
                    ("inf", "-inf*-inf");
                    ("x*x", "|x|*|x|");

                    (* Max *)
                    ("inf", "max {inf, inf}");
                    ("-inf", "max {-inf, -inf}");
                    ("inf", "max {inf, 0}");
                    ("0", "max {0, 0}");
                    ("0", "max{0, 0, -inf}");
                    ("inf", "max{0, 1, inf}");

                    (* Min *)
                    ("inf", "min {inf, inf}");
                    ("-inf", "min {-inf, -inf}");
                    ("-inf", "min {-inf, 0}");
                    ("0", "min {0, 0}");
                    ("-inf", "min{0, 0, -inf}");
                    ("0", "min{0, 1, inf}");

                    (* Combinations *)

                    (* Sum over Product *)
                    (*("5*|x|", "3*|x|+2*|x|");                   
                    ("|x|", "3*|x|-2*|x|"); *)

                    ("4", "max {min {3,7}, min{4,5}}");
                    ("inf", "1 + max {0, 0} + inf + 1 + max {0, 0} + inf");
                    ("0", "max{0, 0}+max{0, 0}");
                  ]
      );
      
    ]

