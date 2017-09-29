open Batteries
open OUnit2

let assert_equal_bool = assert_equal ~printer:Bool.to_string ~cmp:Bool.equal

let assert_equal_string = assert_equal ~printer:identity ~cmp:String.equal

let assert_equal_int = assert_equal ~printer:string_of_int ~cmp:Int.equal
                        
let assert_equal_poly =
  let module P = Polynomials.Make(PolyTypes.OurInt) in
  assert_equal
    ~cmp:P.(=~=)
    ~printer:P.to_string

let assert_equal_bound =
  assert_equal
    ~cmp:Program.TransitionLabel.Bound.(=~=)
    ~printer:Program.TransitionLabel.Bound.to_string
  
let assert_true = assert_bool ""
                
let assert_false b = assert_true (not b)

let assert_exception f =
  assert_true (Result.is_bad (Result.catch f ()))
