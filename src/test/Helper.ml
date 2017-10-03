open Batteries
open OUnit2
open Polynomials
open Formulas
open Constraints
   
(* TODO REDUNDANT in LocalSizeBound
   There is no to_string for options in batteries,
   but there is a very efficient print function which is however a bit inconvenient to use. *)
let to_string_template_bound_option (option: LocalSizeBound.t Option.t): string =
  let output = IO.output_string () in
  Option.print (fun output template_bound -> IO.nwrite output (Bound.to_string (LocalSizeBound.as_bound template_bound))) output option;
  IO.close_out output

let assert_equal_bool = assert_equal ~printer:Bool.to_string ~cmp:Bool.equal

let assert_equal_string = assert_equal ~printer:identity ~cmp:String.equal

let assert_equal_int = assert_equal ~printer:string_of_int ~cmp:Int.equal
                        
let assert_equal_value =
  assert_equal ~cmp:OurInt.(=~=) ~printer:OurInt.to_string

let assert_equal_poly =
  assert_equal ~cmp:Polynomial.(=~=) ~printer:Polynomial.to_string

let assert_equal_parameter_poly =
  assert_equal ~cmp:ParameterPolynomial.(=~=) ~printer:ParameterPolynomial.to_string

let assert_equal_bound =
  assert_equal ~cmp:Bound.(=~=) ~printer:Bound.to_string

let assert_equal_constr =     
  assert_equal ~cmp:Constraint.(=~=) ~printer:Constraint.to_string

let assert_equal_template_bound_option =
  assert_equal ~cmp:(Option.eq ~eq:LocalSizeBound.equal) ~printer:to_string_template_bound_option

let assert_equal_classified_bound =
  assert_equal ~cmp:LocalSizeBound.equal ~printer:LocalSizeBound.to_string

let assert_equal_formula =
  assert_equal
    ~cmp:(fun f1 f2 -> SMT.Z3Solver.unsatisfiable Formula.Infix.(f1 && Formula.neg f2))
    ~printer:Formula.to_string

let assert_true = assert_bool ""
                
let assert_false b = assert_true (not b)

let assert_exception f =
  assert_true (Result.is_bad (Result.catch f ()))
