open Batteries

include Number.MakeNumeric(Big_int)
let (=~=) = equal

(* TODO Make it possible to pow with Big_int *)
let pow i (n: int) = pow i (of_int n)

let max a b =
  if Compare.(a >= b) then
    a
  else
    b

let min a b =
  if Compare.(a <= b) then
    a
  else
    b

let of_ourint =
  identity

let minus_one = sub zero one

let is_zero a =
  equal a zero