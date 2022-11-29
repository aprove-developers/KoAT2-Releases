open Batteries
open Polynomials

module Bound = BoundType.Make_BoundOver(OurInt)

module RealBound =
  struct
    include BoundType.Make_BoundOver(OurFloat)

    let of_intbound =
      Bound.fold
        ~const:(of_constant % OurFloat.of_ourint)
        ~var:(of_var)
        ~plus:(add)
        ~times:(mul)
        ~exp:(fun value -> exp (OurFloat.of_ourint value))
        ~inf:(infinity)

  end
