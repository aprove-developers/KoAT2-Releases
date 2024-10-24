open Koat2
open Batteries
open OUnit2
open Helper

let tests =
  "Bound"
  >::: [
         "simplify"
         >::: List.map
                (fun (expected_bound, bound) ->
                  bound >:: fun _ ->
                  assert_equal_bound (Readers.read_bound expected_bound) (Readers.read_bound bound))
                [
                  (* Inf *)
                  ("inf", "inf");
                  (* Const *)
                  ("0", "0");
                  ("12", "12");
                  (* Var *)
                  ("x", "x");
                  ("y", "y");
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
                  ("2^(x+y)", "2^(x+y)");
                  (* Sum *)
                  ("x", "0+x");
                  ("11", "7+4");
                  ("2*x", "x+x");
                  ("inf", "7+inf");
                  ("inf", "inf+8");
                  ("inf", "inf+inf");
                  (* Product *)
                  ("0", "0*x");
                  ("x", "1*x");
                  ("0", "0*inf");
                  (* Normal forms *)
                  ("x^2+2*x*y+y^2", "(x+y)*(x+y)");
                  ("x^2+2*x*y+y^2+x", "((x+y)*(x+y))+x");
                  ("x^2*y+2*x*y*y+y^3+x*y", "(((x+y)*(x+y))+x)*y");
                  ("10*(x+y)", "10*x + 10*y");
                  (* Sum over Product *)
                  ("5*x", "3*x+2*x");
                  (* Log *)
                  ("log(3*x^2+y)", "2*log(x)+log(y)+2");
                  ("2^(log(x) + log(y))", "x*y");
                  ("3^(log(x) + log(y))", "x*y");
                ];
       ]
