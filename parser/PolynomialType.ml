type t =
  | Constant of int
  | Variable of string
  | Neg of t
  | Plus of t * t
  | Times of t * t
  | Pow of t * int
