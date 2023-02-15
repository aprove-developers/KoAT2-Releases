open Batteries

include Number.MakeNumeric(Num)

let is_integral = Num.is_integer_num
let sum = Enum.fold (+) zero
let list_sum = sum % List.enum

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

let of_ourint = Num.num_of_big_int

let to_ourint_ceiled =
  (* OurInt.of_int (Num.to_int (Num.ceil (add x zero))) (\** TODO maybe we have to add here one*\) *)
  Num.big_int_of_num % Num.ceil

let (>) = Num.(>/)
let (<) = Num.(</)

let (>=) = Num.(>=/)
let (<=) = Num.(<=/)

let minus_one = sub zero one
