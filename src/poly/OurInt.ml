open Batteries

include Number.MakeNumeric(Big_int)
let (=~=) = equal
let pow i (n: int) = pow i (of_int n)  
