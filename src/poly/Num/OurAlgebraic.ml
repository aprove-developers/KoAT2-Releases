open! OurBase
open Koat2_external.Algebraic.Algebraic

type t = real_alg

let zero = zero_ra
let one = one_ra
let neg = uminus_ra
let add = plus_ra
let mul = times_ra
let rec pow a = function
  | 0 -> one
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    let b_sq = mul b b in
    mul b_sq (if n mod 2 = 0 then one else a)
let is_integral = is_rational_ra
let equal = equals_ra
let ( =~= ) = equals_ra
let compare_order = function
    | Eq -> 0
    | Lt -> -1
    | Gt -> 1
let compare x y = compare_ra x y |> compare_order
let of_int = of_integer_ra % OurInt.of_int
let to_string = show_ra
