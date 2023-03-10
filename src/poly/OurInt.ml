open Batteries

let is_zero =
  Z.(equal zero)

let pow_ourint i n =
  let open Z in
  let rec helper i m = if is_zero m then one else mul i (helper i (m - one)) in
  helper i n

include Number.MakeNumeric(struct include Z let modulo = ((mod)) let pow = pow_ourint end)
include Z

let (=~=) = equal

let is_integral _ = true

let of_ourint =
  identity

let is_negative a =
  Compare.(zero > a)

let is_ge =
  Compare.(>=)

let is_gt =
  Compare.(>)

let rec lcm_list = function
  | [] -> one
  | x::xs -> lcm x (lcm_list xs)

let rec max_list = function
  | [] -> zero
  | x::xs -> max x (max_list xs)

let sum_list = List.fold (+) zero
