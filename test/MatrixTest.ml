open Koat2
open Batteries
open OUnit2
open Helper
open! ProgramModules

let of_int = OurInt.of_int

let mat1 =
  OurMatrix.IntMatrix.of_list
    [
      [ of_int 0; of_int 1; of_int 2 ];
      [ of_int (-5); of_int (-3); of_int (-1) ];
      [ of_int 8; of_int 1; of_int (-6) ];
    ]


let mat2 =
  OurMatrix.IntMatrix.of_list [ [ of_int 0; of_int 1 ]; [ of_int (-5); of_int (-3) ]; [ of_int 8; of_int 1 ] ]


let tests =
  "Matrix"
  >::: [
         "to_string"
         >::: List.map
                (fun (expected_string, mat) ->
                  "" >:: fun _ ->
                  let result = OurMatrix.IntMatrix.to_string mat in
                  assert_equal_string expected_string result)
                [ ("[[0; 1; 2]; [-5; -3; -1]; [8; 1; -6]]", mat1); ("[[0; 1]; [-5; -3]; [8; 1]]", mat2) ];
         "mul"
         >::: List.map
                (fun (expected_string, mat) ->
                  "" >:: fun _ ->
                  let result = OurMatrix.IntMatrix.to_string mat in
                  assert_equal_string expected_string result)
                [ ("[[11; -1; -13]; [7; 3; -1]; [-53; -1; 51]]", OurMatrix.IntMatrix.mul mat1 mat1) ];
       ]
