open Batteries
open Polynomials

type t =
  | Infinity
  | Const of OurNum.t
  | Var of Var.t
  | Neg of t
  | Pow of OurNum.t * t
  | Sum of t * t
  | Product of t * t 
  | Cos of t 
  | Sin of t [@@deriving eq, ord]

let rec to_string = function
  | Infinity -> "inf"
  | Const x -> OurNum.to_string x
  | Var x -> Var.to_string x
  | Neg x -> "-" ^ (to_string x)
  | Pow (x,y) -> "(" ^ (OurNum.to_string) x ^ ")^(" ^ (to_string y) ^ ")"
  | Sum (x,y) -> "(" ^ (to_string x) ^ ")+(" ^ (to_string y) ^ ")"
  | Product (x,y) -> "(" ^ (to_string x) ^ ")*(" ^ (to_string y) ^ ")"
  | Cos x -> "cos(" ^ (to_string x) ^ ")"
  | Sin x -> "sin(" ^ (to_string x) ^ ")"
let one = Const OurNum.one
let zero = Const OurNum.zero
let infintiy = Infinity
let const v = Const v
let var v = Var v
let neg b = Neg b
let pow b1 b2 = Pow (b1,b2)
let sum b1 b2 = Sum (b1,b2)

let rec list_sum  = function
  | [] -> zero
  | s::[] -> s
  | s::ss -> Sum (s, list_sum ss)

let option_sum_ = function
  | (None, None) -> Const OurNum.zero
  | (Some x, None) -> x
  | (None, Some x) -> x
  | (Some x, Some y) -> Sum (x,y)
  
let option_sum b1 b2 =
  option_sum_ (b1,b2)

let prod b1 b2 = Product (b1,b2)

let rec list_prod = function
  | [] -> zero
  | f::[] -> f
  | f::fs -> Product (f, list_prod fs)

let cos b = Cos b

let sin b = Sin b
