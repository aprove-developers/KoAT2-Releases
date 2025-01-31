open! OurBase

exception Div_Zero of string

let is_zero = Z.(equal zero)

(** [n] must be non-negative. Raises Div_Zero for [0^0] *)
let pow_ourint b n =
  let open Z in
  let rec helper b n =
    if is_zero n then
      one
    else
      let res = helper b (div n (of_int 2)) in
      let res_sq = mul res res in
      mul res_sq
        (if n mod of_int 2 = zero then
           one
         else
           b)
  in
  helper b n


include Z
include Z.Compare

let ( =~= ) = equal
let is_integral _ = true
let of_ourint = identity
let is_negative a = Compare.(zero > a)
let is_ge = Compare.( >= )
let is_gt = Compare.( > )
let log x = of_int (Z.log2up x)

(* Use as [range start ~inc stop. Both ends are inclusive] *)
let range_from ?(inc = one) start = Sequence.unfold ~init:start ~f:(fun next -> Some (next, next + inc))

let range start ?(inc = one) stop =
  Sequence.unfold ~init:(range_from ~inc start) ~f:(fun range_from ->
      let open OptionMonad in
      let* number, rem = Sequence.next range_from in
      Option.some_if (number <= stop) (number, rem))


let all_nonnegative = range_from zero

let rec lcm_list = function
  | [] -> one
  | x :: xs -> lcm x (lcm_list xs)


let rec max_list = function
  | [] -> zero
  | x :: xs -> max x (max_list xs)


(** [ binomial n k ] computes the binomial coefficient choose(n,k) *)
let rec binomial n k =
  if is_gt k n then
    zero
  else if equal n k then
    one
  else
    n * binomial (n - one) k / (n - k)


let sum_list = List.fold ~f:( + ) ~init:zero
