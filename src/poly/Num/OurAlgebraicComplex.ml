open! OurBase
open Koat2_external.Algebraic.Algebraic

type t = complex

let zero = zero_ca
let one = one_ca
let neg = uminus_ca
let add = plus_ca
let mul = times_ca
let rec pow a = function
  | 0 -> one
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    let b_sq = mul b b in
    mul b_sq (if n mod 2 = 0 then one else a)
let equal = equals_ca
let ( =~= ) = equals_ca
let is_integral x =
  OurAlgebraic.(imag_of_ca x =~= zero) && OurAlgebraic.(is_integral @@ real_of_ca x)
let compare_order = function
    | Eq -> 0
    | Lt -> -1
    | Gt -> 1
let compare x y =
  let to_tuple x = (real_of_ca x, imag_of_ca x) in
  Tuple2.compare ~cmp1:OurAlgebraic.compare ~cmp2:OurAlgebraic.compare (to_tuple x) (to_tuple y)
let of_int = of_integer_ca % OurInt.of_int
let to_string = show_ca
