open Batteries
open Koat2
open OUnit2
open Helper
open Formulas
open Polynomials
open ProbabilisticProgramModules

(* TODO more cases once moment computation in ProbabilityDistribution has been further extended *)
let tests =
  "ProbabilityDistributionTests" >::: [
    "moment_poly" >:::
        List.map
          (fun (order, dist_str, expected_result) ->
             let order_string = if order = 1 then "" else "^" ^ Int.to_string order in
             Printf.sprintf "E(%s^%s)" dist_str order_string >:: fun _ ->
             (* "E(" ^ dist_str ^ order_string ^ ")" >:: fun _ -> *)
               let dist = Readers.read_probability_distribution dist_str in
               let result = ProbabilityDistribution.moment_poly dist order in

               assert_equal_realpoly_smt expected_result result
          )
          (let open RealPolynomial in
            [

              ( 1
              , "UNIFORM(0,5)"
              , of_constant (OurFloat.of_float 2.5) )
            ; ( 1
              , "UNIFORM(-5,-2)"
              , of_constant (OurFloat.of_float (-3.5)) )
            ; ( 1
              , "UNIFORM(-3,5)"
              , of_constant (OurFloat.of_float 1.) )
            ; ( 1
              , "UNIFORM(-3,2*X)"
              , of_var (Var.of_string "X") - of_constant (OurFloat.of_float 1.5) )

            ; ( 2
              , "UNIFORM(0,3)"
              , of_constant (OurFloat.of_float 3.5) )
            ; ( 2
              , "UNIFORM(-3,4)"
              , of_constant (OurFloat.of_float 5.5) )
            ; ( 2
              , "UNIFORM(-3,2*X)"
              , (of_constant OurFloat.(one / of_int 6)) * of_intpoly (Readers.read_polynomial "8*X^2 - 10*X + 21"))
            ]);

    "moment_abs_bound" >:::
        List.map
          (fun (order, dist_str, expected_result) ->
             let order_string = if order = 1 then "" else "^" ^ Int.to_string order in
             Printf.sprintf "E(%s^%s)" dist_str order_string >:: fun _ ->
               let dist = Readers.read_probability_distribution dist_str in
               let result = ProbabilityDistribution.moment_abs_bound dist order in

               assert_ge_realbound_smt result expected_result
          )
          (let open BoundsInst.RealBound in
            [

              ( 1
              , "UNIFORM(0,5)"
              , of_constant (OurFloat.of_float 2.5) )
            ; ( 1
              , "UNIFORM(-5,-2)"
              , of_constant (OurFloat.of_float (-3.5)) )
            ; ( 1
              , "UNIFORM(-3,5)"
              , of_constant OurFloat.(of_float 1. + one / of_int 3) )
            (* The next expected result is not a tight bound since it can not be express as a polynomial *)
            ; ( 1
              , "UNIFORM(-3,2*X)"
              , of_var (Var.of_string "X") +of_constant OurFloat.(of_float 1.5))

            ; ( 2
              , "UNIFORM(0,3)"
              , of_constant (OurFloat.of_float 3.5) )
            ; ( 2
              , "UNIFORM(-3,4)"
              , of_constant (OurFloat.of_float 5.5) )
            ; ( 2
              , "UNIFORM(-3,2*X)"
              , (of_constant OurFloat.(one / of_int 6)) * of_intpoly (Readers.read_polynomial "8*X^2 - 10*X + 21"))
            ])
  ]
