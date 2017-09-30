open Batteries
open OUnit2

let assert_equal_bool = assert_equal ~printer:Bool.to_string ~cmp:Bool.equal

let assert_equal_string = assert_equal ~printer:identity ~cmp:String.equal

let assert_equal_int = assert_equal ~printer:string_of_int ~cmp:Int.equal
                        
let assert_equal_poly =
  let module P = Polynomials.Make(PolyTypes.OurInt) in
  assert_equal ~cmp:P.(=~=) ~printer:P.to_string

let assert_equal_bound =
  assert_equal ~cmp:Bound.(=~=) ~printer:Bound.to_string

let assert_equal_classified_bound =
  assert_equal ~cmp:LocalSizeBound.equal ~printer:LocalSizeBound.to_string

let assert_equal_formula =
  let module F = Formula.Make(Polynomials.Make(PolyTypes.OurInt)) in
  assert_equal
    ~cmp:(fun f1 f2 -> SMT.Z3Solver.unsatisfiable F.Infix.(f1 && F.neg f2))
    ~printer:F.to_string

let assert_true = assert_bool ""
                
let assert_false b = assert_true (not b)

let assert_exception f =
  assert_true (Result.is_bad (Result.catch f ()))
