open Batteries

include Number.MakeNumeric(Float)
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
  |> OurInt.to_float 
  |> of_float
