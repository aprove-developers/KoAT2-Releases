open Batteries

include Polynomials.Make(PolyTypes.OurInt)

let separate_by_sign poly =
  partition (fun scaled -> PolyTypes.OurInt.Compare.(ScaledMonomial_.coeff scaled >= PolyTypes.OurInt.zero)) poly

