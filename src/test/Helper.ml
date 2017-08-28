open Batteries
open OUnit2

let assert_equal_bool = assert_equal ~printer:Bool.to_string ~cmp:Bool.equal

let assert_equal_string = assert_equal ~printer:identity ~cmp:String.equal

let assert_true = assert_bool ""
                
let assert_false b = assert_true (not b)

let assert_exception f =
  assert_true (Result.is_bad (Result.catch f ()))
