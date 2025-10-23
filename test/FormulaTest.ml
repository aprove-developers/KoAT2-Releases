open Koat2
open Batteries
open OUnit2
open Helper
open Formulas

let tests =
  "Formula"
  >::: [
         "le_than_any"
         >::: List.map
                (fun (expected, lhs, rhs) ->
                  lhs ^ " < " ^ "max{" ^ String.concat "," rhs ^ "}" >:: fun _ ->
                  assert_equal_formula (KoatReaders.read_formula expected)
                    (Formula.le_than_any (KoatReaders.read_polynomial lhs)
                       (List.map KoatReaders.read_polynomial rhs)))
                [ ("0 = 1", "x", []); ("x <= y", "x", [ "y" ]); ("x <= y || x <= z", "x", [ "y"; "z" ]) ];
       ]
