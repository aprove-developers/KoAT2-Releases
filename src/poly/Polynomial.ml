open Batteries

include Polynomials.Make(OurInt)

let separate_by_sign poly =
  partition (fun scaled -> OurInt.Compare.(ScaledMonomial_.coeff scaled >= OurInt.zero)) poly

