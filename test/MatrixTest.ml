open Koat2
open OurBase
open OUnit2
open Helper
open! ProgramModules
open OurMatrix.IntMatrix

let mat1 = of_list @@ List.map ~f:(List.map ~f:OurInt.of_int) [ [ 0; 1; 2 ]; [ -5; -3; -1 ]; [ 8; 1; -6 ] ]
let mat2 = of_list @@ List.map ~f:(List.map ~f:OurInt.of_int) [ [ 0; 1 ]; [ -5; -3 ]; [ 8; 1 ] ]
let mat3 = of_list @@ List.map ~f:(List.map ~f:OurInt.of_int) [ [ 3; 2 ]; [ -5; -3 ] ]

let mat4 =
  of_list
  @@ List.map ~f:(List.map ~f:OurInt.of_int)
       [ [ 1; 0; 0; 0; 0 ]; [ 3; 1; 0; 0; 0 ]; [ 6; 3; 2; 0; 0 ]; [ 10; 6; 3; 2; 0 ]; [ 15; 10; 6; 3; 2 ] ]


let mat5 =
  of_list
  @@ List.map ~f:(List.map ~f:OurInt.of_int)
       [ [ 5; 1; -2; 4 ]; [ 0; 5; 2; 2 ]; [ 0; 0; 5; 3 ]; [ 0; 0; 0; 4 ] ]


let mat6 = of_list @@ List.map ~f:(List.map ~f:OurInt.of_int) [ [ 5; 1; -10 ]; [ 0; 5; 0 ]; [ 0; 0; 5 ] ]

let tests =
  "Matrix"
  >::: [
         "to_string"
         >::: List.map
                ~f:(fun (expected_string, mat) ->
                  "" >:: fun _ ->
                  let result = to_string mat in
                  assert_equal_string expected_string result)
                [ ("[[0; 1; 2]; [-5; -3; -1]; [8; 1; -6]]", mat1); ("[[0; 1]; [-5; -3]; [8; 1]]", mat2) ];
         "mul"
         >::: List.map
                ~f:(fun (expected_string, mat) ->
                  "" >:: fun _ ->
                  let result = to_string mat in
                  assert_equal_string expected_string result)
                [ ("[[11; -1; -13]; [7; 3; -1]; [-53; -1; 51]]", mul mat1 mat1) ];
         "eigenvalues"
         >::: List.map
                ~f:(fun (expected_string, mat) ->
                  "" >:: fun _ ->
                  let result = eigenvalues mat in
                  assert_equal_string expected_string
                    (Util.sequence_to_string (Sequence.of_list result) ~f:OurAlgebraicComplex.to_string))
                [
                  ("[0; -1; -8]", mat1); ("[1i; -1i]", mat3); ("[2; 1]", mat4); ("[5; 4]", mat5); ("[5]", mat6);
                ];
         "JNF"
         >::: List.map
                ~f:(fun (expected_string_J, expected_string_P, mat) ->
                  "" >:: fun _ ->
                  let p, j, p_inv = jordan_normal_form mat in
                  assert_true OurMatrix.CAMatrix.(equal (mul p p_inv) (identity (dim_col mat)));
                  assert_true OurMatrix.CAMatrix.(equal (mul p_inv p) (identity (dim_col mat)));
                  assert_true OurMatrix.CAMatrix.(equal (mul p (mul j p_inv)) (convertMatrixToCA mat)))
                [
                  (* ("", "[0; -1; -8]", mat1);
                     ("", "[1i; -1i]", mat3);
                     ("", "[2; 1]", mat4); *)
                  ("[[4; 0; 0; 0]; [0; 5; 1; 0]; [0; 0; 5; 1]; [0; 0; 0; 5]]", "[5; 4]", mat5);
                ];
       ]
