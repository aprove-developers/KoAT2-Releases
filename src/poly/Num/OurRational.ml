open! OurBase

exception Div_Zero of string

include Q

let product = Sequence.fold ~init:Q.one ~f:Q.mul
let is_integral t = OurInt.equal OurInt.one t.den
let sum = Sequence.fold ~f:( + ) ~init:zero
let list_sum = sum % Sequence.of_list
let ( =~= ) = equal
let of_ourint = of_bigint

exception NotConvertibleInteger of string

(** Floors the result *)
let to_ourint t =
  if true || is_integral t then
    Z.div t.num t.den
  else
    raise (NotConvertibleInteger (Q.to_string t))


let minus_one = sub zero one

let pow_ourint b e =
  match Z.(compare e zero) with
  | 0 -> one
  | -1 ->
      let e_non_neg = OurInt.abs e in
      make (OurInt.pow_ourint b.den e_non_neg) (OurInt.pow_ourint b.num e_non_neg)
  | _ -> make (OurInt.pow_ourint b.num e) (OurInt.pow_ourint b.den e)


let pow b e = pow_ourint b Z.(of_int e)
let ( ** ) = pow

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
