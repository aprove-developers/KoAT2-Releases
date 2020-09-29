open Batteries
open Polynomials

module Bound = BoundType.Make_BoundOver (OurInt) (Polynomial)

module RealBound = 
  struct
    include BoundType.Make_BoundOver (OurFloat) (RealPolynomial)

    let of_intbound = 
      Bound.fold 
        ~const:(of_constant % OurFloat.of_ourint) 
        ~var:(of_var)
        ~neg:(neg)
        ~plus:(add)
        ~times:(mul)
        ~exp:(fun value -> exp (OurFloat.of_ourint value))
        ~max:(max)
        ~inf:(infinity)

  end
