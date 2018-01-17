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
