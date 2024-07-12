open! OurBase
open Koat2_external.Algebraic.Algebraic

type t = complex

let zero = zero_ca
let one = one_ca
let imag_unit = imag_unit_ca
let of_algebraic = of_real_imag_ca
let imag = imag_of_ca
let real = real_of_ca
let add = plus_ca
let ( + ) = add
let sum = Sequence.fold ~init:one ~f:add
let neg = uminus_ca
let ( ~- ) = neg
let sub x y = add x (-y)
let ( - ) = sub
let mul = times_ca
let product = Sequence.fold ~init:one ~f:mul
let ( * ) = mul
let inv = inverse_ca
let div x y = mul x (inv y)

let rec pow a = function
  | 0 -> one
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      let b_sq = mul b b in
      mul b_sq
        (if n mod 2 = 0 then
           one
         else
           a)


let ( ** ) = pow
let equal = equals_ca
let ( =~= ) = equals_ca
let is_integral x = OurAlgebraic.(imag_of_ca x =~= zero) && OurAlgebraic.(is_integral @@ real_of_ca x)
let abs x = OurAlgebraic.(sqrt @@ add (pow (real x) 2) (pow (imag x) 2))

let compare_order = function
  | Eq -> 0
  | Lt -> -1
  | Gt -> 1


let compare x y =
  let to_tuple x = (real_of_ca x, imag_of_ca x) in
  Tuple2.compare ~cmp1:OurAlgebraic.compare ~cmp2:OurAlgebraic.compare (to_tuple x) (to_tuple y)


let of_int = of_integer_ca % OurInt.of_int
let of_ourint = of_integer_ca
let to_string = show_ca
