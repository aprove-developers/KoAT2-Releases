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

let upper_int x =
  OurInt.of_int (Num.to_int (Num.ceil (add x zero))) (** TODO maybe we have to add here one*)

let (>) = Num.(>/)
let (<) = Num.(</)

let (>=) = Num.(>=/)
let (<=) = Num.(<=/)

let minus_one = sub zero one
