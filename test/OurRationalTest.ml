open Koat2
open Batteries
open OUnit2
open Helper
open! ProgramModules

let tests =
  "OurRational"
  >::: [
         "equal"
         >::: List.map
                (fun (expected_bool, tuple) ->
                  "" >:: fun _ ->
                  let result =
                    OurRational.equal (OurRational.of_intfraction (2, 42)) (OurRational.of_intfraction tuple)
                  in
                  assert_equal_bool expected_bool result)
                [ (true, (2, 42)); (false, (2, 43)); (false, (3, 42)); (false, (0, 43)) ];
         "to_string"
         >::: List.map
                (fun (expected_string, tuple) ->
                  "" >:: fun _ ->
                  let result = OurRational.to_string (OurRational.of_intfraction tuple) in
                  assert_equal_string expected_string result)
                [
                  ("-1/21", (-2, 42));
                  ("0", (0, 43));
                  ("-1", (-1, 1));
                  ("2", (42, 21));
                  ("-1/4", (-1, 4));
                  ("1/4", (-2, -8));
                ];
         "mul"
         >::: List.map
                (fun (expected_string, tuple1, tuple2) ->
                  "" >:: fun _ ->
                  let result =
                    OurRational.mul (OurRational.of_intfraction tuple1) (OurRational.of_intfraction tuple2)
                    |> OurRational.to_string
                  in
                  assert_equal_string expected_string result)
                [
                  ("0", (0, 42), (5, 3));
                  ("5/43", (1, 43), (10, 2));
                  ("-2", (-4, 1), (1, 2));
                  ("1/4", (1, 2), (1, 2));
                  ("1/4", (-1, 2), (1, -2));
                  ("1/4", (-1, 2), (-1, 2));
                  ("1/4", (1, -2), (-1, 2));
                  ("-1/16", (-2, 8), (1, 4));
                  ("-1/16", (2, -8), (1, 4));
                ];
         "add"
         >::: List.map
                (fun (expected_string, tuple1, tuple2) ->
                  "" >:: fun _ ->
                  let result =
                    OurRational.add (OurRational.of_intfraction tuple1) (OurRational.of_intfraction tuple2)
                    |> OurRational.to_string
                  in
                  assert_equal_string expected_string result)
                [
                  ("5/3", (0, 42), (5, 3));
                  ("216/43", (1, 43), (10, 2));
                  ("-7/2", (-4, 1), (1, 2));
                  ("3", (5, 2), (1, 2));
                  ("-1", (-1, 2), (1, -2));
                  ("-1", (-1, 2), (-1, 2));
                  ("-1", (1, -2), (-1, 2));
                  ("0", (-2, 8), (1, 4));
                  ("0", (2, -8), (1, 4));
                ];
         "is_negative"
         >::: List.map
                (fun (expected_bool, tuple) ->
                  "" >:: fun _ ->
                  let result = OurRational.(OurRational.of_intfraction tuple < zero) in
                  assert_equal_bool expected_bool result)
                [
                  (true, (-2, 42));
                  (false, (0, 43));
                  (true, (-1, 1));
                  (false, (42, 21));
                  (true, (-1, 4));
                  (false, (-2, -8));
                  (false, (-42, -1));
                ];
         "compare"
         >::: List.map
                (fun (expected_int, tuple1, tuple2) ->
                  "" >:: fun _ ->
                  let result =
                    OurRational.compare (OurRational.of_intfraction tuple1)
                      (OurRational.of_intfraction tuple2)
                  in
                  assert_equal_int expected_int result)
                [
                  (-1, (0, 42), (5, 3));
                  (-1, (1, 43), (10, 2));
                  (-1, (-4, 1), (1, 2));
                  (1, (5, 2), (1, 2));
                  (0, (-1, 2), (1, -2));
                  (0, (-1, 2), (-1, 2));
                  (0, (1, -2), (-1, 2));
                  (-1, (-2, 8), (1, 4));
                  (1, (2, 8), (1, -4));
                ];
         "is_ge"
         >::: List.map
                (fun (expected_bool, tuple1, tuple2) ->
                  "" >:: fun _ ->
                  let result = OurRational.(of_intfraction tuple1 >= of_intfraction tuple2) in
                  assert_equal_bool expected_bool result)
                [
                  (false, (0, 42), (5, 3));
                  (false, (1, 43), (10, 2));
                  (false, (-4, 1), (1, 2));
                  (true, (5, 2), (1, 2));
                  (true, (-1, 2), (1, -2));
                  (true, (-1, 2), (-1, 2));
                  (true, (1, -2), (-1, 2));
                  (false, (-2, 8), (1, 4));
                  (true, (2, 8), (1, -4));
                ];
         "pow"
         >::: List.map
                (fun (expected_string, tuple1, exp) ->
                  "" >:: fun _ ->
                  let result =
                    OurRational.pow (OurRational.of_intfraction tuple1) exp |> OurRational.to_string
                  in
                  assert_equal_string expected_string result)
                [
                  ("1", (1, 1), 1);
                  ("125", (5, 1), 3);
                  ("1", (5, 1), 0);
                  ("0", (0, 3), 42);
                  ("1/8", (1, 2), 3);
                  ("-1/8", (-1, 2), 3);
                  ("-1/8", (1, -2), 3);
                  ("1/16", (1, -2), 4);
                  ("2401/625", (7, 5), 4);
                ];
         "ceil"
         >::: List.map
                (fun (expected_string, tuple) ->
                  "" >:: fun _ ->
                  let result = OurRational.ceil (OurRational.of_intfraction tuple) |> OurInt.to_string in
                  assert_equal_string expected_string result)
                [ ("1", (3, 5)); ("0", (-3, 5)); ("7", (49, 7)) ];
         "floor"
         >::: List.map
                (fun (expected_string, tuple) ->
                  "" >:: fun _ ->
                  let result = OurRational.floor (OurRational.of_intfraction tuple) |> OurInt.to_string in
                  assert_equal_string expected_string result)
                [ ("0", (3, 5)); ("-1", (-3, 5)); ("7", (49, 7)) ];
       ]
