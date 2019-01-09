open Batteries

include Number.MakeNumeric(Num)

let (=~=) = equal

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

let of_ourint f =
  f  
  |> Num.num_of_big_int

let of_float_string fs =
  fs |> Num.of_float_string

let (>) = Num.(>/)
let (<) = Num.(</)

let (>=) = Num.(>=/)
let (<=) = Num.(<=/)
