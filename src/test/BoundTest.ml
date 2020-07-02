open Batteries
open OUnit2
open Helper
open BoundsInst

type comperator = [`GT | `GE]

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
                    ("3+2*x", "3+x+x");

                    (* Minus *)
                    ("-x", "0-x");
                    ("3", "7-4");
                    ("0", "|x|-|x|");
                    ("-inf", "7-inf");
                    ("inf", "inf-8");
                    ("-inf", "-inf-8");
                    ("3-2*x", "3-x-x");

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
                    ("-2*|x|*inf", "-|x|*inf");
                    (* Not supported ("x*x", "|x|*|x|"); *)

                    (* Max *)
                    ("inf", "max {inf, inf}");
                    ("-inf", "max {-inf, -inf}");
                    ("inf", "max {inf, 0}");
                    ("0", "max {0, 0}");
                    ("0", "max{0, 0, -inf}");
                    ("inf", "max{0, 1, inf}");
                    ("2*|X|", "max{0,2*|X|}");
                    ("Y + max{0,X}", "max{Y, Y+max{0, X}}");

                    (* Min *)
                    ("inf", "min {inf, inf}");
                    ("-inf", "min {-inf, -inf}");
                    ("-inf", "min {-inf, 0}");
                    ("0", "min {0, 0}");
                    ("-inf", "min{0, 0, -inf}");
                    ("0", "min{0, 1, inf}");
                    ("0","min{0,2*|X|}");

                    (* Combinations *)

                    (* Sum over Product *)
                    ("5*|x|", "3*|x|+2*|x|");
                    ("|x|", "3*|x|-2*|x|");

                    ("4", "max {min {3,7}, min{4,5}}");
                    ("inf", "1 + max {0, 0} + inf + 1 + max {0, 0} + inf");
                    ("0", "max{0, 0}+max{0, 0}");

                    ("max {|X|,2}", "max {|X|, max {|X|,2}}");
                    ("max {2*|X|,2}", "max {|X|, max {2*|X|,2}}");
                    ("min {|X|,2}", "min {|X|, min {|X|,2}}");
                    ("min {|X|,2}", "min {|X|, min {2*|X|,2}}");
                    ("15 * |X|", "3 * |5 * X|");
                    ("15 * X * X * X * Y", "X * 3 * X * X * 5 * Y");

                    ("|X|", "max {min {2*|X|,|X|}, |X|}");
                    ("|X|", "min {max {2*|X|,|X|}, |X|}");

                    ("-2 * |X|", "max {-3 * |X|, -2 * |X|}");
                    ("-3 * |X|", "min {-3 * |X|, -2 * |X|}");

                    ("X", "X + X - X + X + X - X - X")
                  ]
      );

      "linearity" >:::
        List.map
          (fun (linear, v, bound_string) ->
            bound_string >::
              (fun _ ->
                let bound = Readers.read_bound bound_string in
                let var = Var.of_string v in
                let error_string =
                  "Linearity Mismatch Expected " ^ bound_string ^ " to " ^
                  (if linear then " be linear " else " not be linear ") ^
                  " in " ^ v ^ " but the opposite is the case"
                in
                assert_bool error_string (Bound.is_linear_in_var var bound = linear)
              )
          )
          [
            (true,  "x", "x");
            (true,  "x", "2*x");
            (true,  "x", "2*x+y");
            (true,  "y", "2*x+y");
            (true,  "y", "2*x+y + 0");
            (true,  "y", "2*x+y + 0 * y * y");

            (true,  "y", "|0|");
            (true,  "y", "|x|");
            (false, "x", "|x|");

            (true,  "x", "max {y,z}");
            (false, "y", "max {y,z}");
            (false, "z", "max {y,z}");

            (true,  "x", "min {y,z}");
            (false, "y", "min {y,z}");
            (false, "z", "min {y,z}");

            (false, "x", "x^2");
            (true,  "y", "x^2");
          ];

          "comparison" >:::
            List.map
              (
                fun (b1,cmp,b2,expres) ->
                  let op_bool_eq = curry @@ function
                    | (Some b1, Some b2) -> Bool.equal b1 b2
                    | (None, None)       -> true
                    | _                  -> false
                  in
                  let print_op_bool = function
                    | Some b -> "Some " ^ Bool.to_string b
                    | None   -> "None"
                  in
                  let cmp_function = match cmp with
                    | `GT -> Bound.(>)
                    | `GE -> Bound.(>=)
                  in
                  let cmp_string = match cmp with
                    | `GT -> ">"
                    | `GE -> ">="
                  in
                  let res = cmp_function (Readers.read_bound b1) (Readers.read_bound b2) in
                  let descr = b1 ^ " " ^ cmp_string ^ " " ^ b2 in
                  descr >::
                    (fun _ ->
                      let error_string =
                        descr ^ " Expected: " ^ print_op_bool expres ^ " got: " ^ print_op_bool res
                      in
                      assert_bool error_string (op_bool_eq res expres)
                    )

              )
              [
                "1", `GT,"0", Some true;
                "-2 + -2", `GE,"-3", Some false;
                "-2 + -5", `GE,"-10", Some true;
                "|X|", `GT,"0", None;
                "|X|", `GE,"0", Some true;

                "max{|X|,-1}", `GE, "0", Some true;
                "max{|X|,-1}", `GT, "0", None;
                "min{|X|,-1}", `GE, "-1", Some true;
                "min{|X|,-1}", `GT, "-1", Some false;

                "0", `GT, "min{|X|,-1}", Some true;
                "0", `GT, "min{|X|,-1}", Some true;

              ]
    ]

