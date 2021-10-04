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

let min a b =
  if Compare.(a <= b) then
    a
  else
    b

let of_ourint =
  identity

let minus_one = sub zero one

let is_zero a =
  equal a zero

let is_negative a =
  compare zero a > 0

let is_ge a b =
  compare a b >= 0

let is_gt a b =
  compare a b > 0

let pow_ourint i n = 
    let rec helper i m = if is_zero m then one else mul i (helper i (m - one)) in
    helper i n

let rec gcd a b =
  if is_zero b then abs a
  else gcd b (modulo a b)

let lcm a b = 
  if is_zero a || is_zero b then zero
  else abs (mul a (div b (gcd a b)))

let rec lcm_list = function
  | [] -> one
  | x::xs -> lcm x (lcm_list xs)

let rec max_list = function
  | [] -> zero
  | x::xs -> max x (max_list xs)
