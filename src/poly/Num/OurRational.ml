open! OurBase

exception Div_Zero of string

(** [n] must be non-negative. Raises Div_Zero for [0^0] *)
let pow_ourint b e =
  let open Q in
  let rec pow_ourint_tail_rec_pos_e acc (e : Z.t) =
    if Z.(equal e zero) then
      acc
    else
      pow_ourint_tail_rec_pos_e (acc * b) Z.(e - one)
  in
  if b = zero then
    if Z.(equal zero e) then
      raise (Div_Zero "OurRational.pow_ourint 0^0")
    else
      zero
  else if Z.Compare.(e >= Z.zero) then
    pow_ourint_tail_rec_pos_e one e
  else
    one / pow_ourint_tail_rec_pos_e one (Z.neg e)


include Q

let pow b e = pow_ourint b Z.(of_int e)
let is_integral t = OurInt.equal OurInt.one t.den
let sum = Sequence.fold ~f:( + ) ~init:zero
let list_sum = sum % Sequence.of_list
let ( =~= ) = equal
let of_ourint = of_bigint

(** Floors the result *)
let to_ourint t = Z.fdiv t.num t.den

let minus_one = sub zero one

module Compare = struct
  let ( < ) = ( < )
  let ( <= ) = ( <= )
  let ( >= ) = ( >= )
  let ( > ) = ( > )
end

include Compare

let floor t = Z.fdiv t.num t.den

let ceil t =
  Z.(
    floor t
    +
    if Z.equal zero (t.num mod t.den) then
      zero
    else
      one)


let log x = of_ourint @@ OurInt.log @@ ceil x
let of_intfraction (num, den) = make (Z.of_int num) (Z.of_int den)