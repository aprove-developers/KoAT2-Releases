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

let one = Const OurNum.one
let zero = Const OurNum.zero
let infintiy = Infinity
let const v = Const v
let var v = Var v
let neg b = Neg b
let pow b1 b2 = Pow (b1,b2)
let sum b1 b2 = Sum (b1,b2)

let rec list_sum summands =
  match summands with
    | [] -> zero
    | s::[] -> s
    | s::ss -> Sum (s, list_sum ss)

let prod b1 b2 = Product (b1,b2)

let rec list_prod factors =
  match factors with
    | [] -> zero
    | f::[] -> f
    | f::fs -> Product (f, list_prod fs)

let cos b = Cos b

let sin b = Sin b
