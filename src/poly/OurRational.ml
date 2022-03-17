open Batteries

(* Momentan ignorieren wir Nenner die 0 sind. *)

exception Div_Zero of string

type t = OurInt.t * OurInt.t

let equal (n1, d1) (n2, d2) = (OurInt.equal n1 n2) && (OurInt.equal d1 d2)

let (=~=) = equal

let zero = (OurInt.zero, OurInt.one)

let is_zero (n,d) = OurInt.is_zero n

let one = (OurInt.one, OurInt.one)

let minus_one = (OurInt.minus_one, OurInt.one)

let reduce_sign (n, d) =
    let sign = if ((OurInt.is_negative n) && (OurInt.is_negative d))
               || ((not (OurInt.is_negative n)) && (OurInt.is_negative d)) then OurInt.minus_one else OurInt.one in
    (OurInt.mul n sign, OurInt.mul d sign)

let reduce (n, d) =
    let gcd = OurInt.gcd n d in
    (OurInt.div n gcd, OurInt.div d gcd) |> reduce_sign

let of_int i =
    (OurInt.of_int i, OurInt.one)

let of_int_tuple (n,d) =
    if d == 0 then raise (Div_Zero "OurRational.of_ourint div_zero") else (OurInt.of_int n, OurInt.of_int d) |> reduce

let to_int (n,d) = OurInt.div n d |> OurInt.to_int

let to_ourint (n,d) = OurInt.div n d

let of_ourint n = (n, OurInt.one)

let of_ourint_tuple (n,d) = if OurInt.is_zero d then raise (Div_Zero "OurRational.of_ourint div_zero") else (n,d) |> reduce

let to_ourfloat (n,d) = OurFloat.(div (of_ourint n) (of_ourint d))


let nom = Tuple2.first
let den = Tuple2.second

let to_string (n,d) =
    if OurInt.(equal one d) then OurInt.to_string n
    else if OurInt.is_zero n then "0"
    else (OurInt.to_string n) ^ "/" ^ (OurInt.to_string d)

let mul (n1, d1) (n2, d2) =
    let (n3, d3) = reduce (n2, d1) in
    let (n4, d4) = reduce (n1, d2) in
    (OurInt.mul n3 n4, OurInt.mul d3 d4) |> reduce_sign

let add (n1, d1) (n2, d2) =
    if OurInt.is_zero n1 then (n2, d2)
    else if OurInt.is_zero n2 then (n1, d1)
    else
        let gcd_d = OurInt.gcd d1 d2 in
        (OurInt.(add (mul n1 (div d2 gcd_d)) (mul n2 (div d1 gcd_d))), (OurInt.lcm d1 d2)) |> reduce

let neg (n,d) = (OurInt.neg n, d) |> reduce_sign

let sub r1 r2 = add r1 (neg r2)

let is_negative (n,d) = (OurInt.is_negative n && not (OurInt.is_negative d)) || (OurInt.is_negative d && not (OurInt.is_negative n))

let compare r1 r2 = let diff = sub r1 r2 in
    if is_negative diff then -1
    else if is_zero diff then 0
    else 1

let is_ge a b =
  compare a b >= 0

let reciprocal (n,d) = if OurInt.is_zero n then raise (Div_Zero "OurRational.reciprocal div_zero") else (d,n)

let div r1 r2 = if is_zero r2 then raise (Div_Zero "OurRational.div div_zero") else mul r1 (reciprocal r2)


let pow_ourint i n =
    if (is_zero i) && not (OurInt.is_zero n) then
        zero
    else if (is_zero i) && (OurInt.is_zero n) then
        raise (Div_Zero "OurRational.pow_ourint div_zero")
    else
        let rec helper i m = if OurInt.is_zero m then one else mul i (helper i OurInt.(m - one)) in
        if OurInt.is_negative n then helper i (OurInt.abs n) |> reduce_sign |> reciprocal
        else helper i n |> reduce_sign

let pow i (n: int) = pow_ourint i (OurInt.of_int n)

let is_integer = (OurInt.equal OurInt.one) % Tuple2.second % reduce

let (-) = sub
let (+) = add

let sign (n,d) = OurInt.sign n * OurInt.sign d

let abs (n,d) = OurInt.abs n, OurInt.abs d

let ceil (n,d) = OurInt.(add (div n d) (if is_zero (modulo n d) then zero else one))

let floor (n,d) = OurInt.div n d
