open Batteries
open OUnit2

let assert_equal_bool = assert_equal ~printer:Bool.to_string ~cmp:Bool.equal

let assert_equal_string = assert_equal ~printer:identity ~cmp:String.equal

let assert_equal_int = assert_equal ~printer:string_of_int ~cmp:Int.equal
                        
let assert_equal_poly =
  assert_equal
    ~cmp:ProgramImpl.StdProgram.Constraint_.Atom_.Polynomial_.(=~=)
    ~printer:ProgramImpl.StdProgram.Constraint_.Atom_.Polynomial_.to_string

let assert_equal_bound =
  assert_equal
    ~cmp:ProgramImpl.StdProgram.TransitionLabel.Bound.(=~=)
    ~printer:ProgramImpl.StdProgram.TransitionLabel.Bound.to_string
  
let assert_true = assert_bool ""
                
let assert_false b = assert_true (not b)

let assert_exception f =
  assert_true (Result.is_bad (Result.catch f ()))
