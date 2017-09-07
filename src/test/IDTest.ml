open Batteries
open OUnit2
open Helper

let tests =
  "Fresh vars" >:: (
    fun _ ->
    let vars1 = ID.StringID.fresh_id_list 3 in
    let vars2 = ID.StringID.fresh_id_list 7 in
    let vars3 = ID.StringID.fresh_id_list 4 in
    let all_vars = Set.of_list (List.flatten [vars1; vars2; vars3]) in
    assert_equal 14 (Set.cardinal all_vars)
  )
