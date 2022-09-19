open Koat2
open Batteries
open OUnit2
open Helper

let tests =
  "Fresh vars" >:: (
    fun _ ->
    let vars1 = Var.fresh_id_list Var.Int 3 in
    let vars2 = Var.fresh_id_list Var.Int 7 in
    let vars3 = Var.fresh_id_list Var.Int 4 in
    let all_vars = Set.of_list (List.flatten [vars1; vars2; vars3]) in
    assert_equal 14 (Set.cardinal all_vars)
  )
