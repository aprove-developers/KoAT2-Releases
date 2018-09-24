open Polynomials

module Bound = BoundType.Make_BoundOver (OurInt) (Polynomial)
module RealBound = BoundType.Make_BoundOver (OurFloat) (RealPolynomial)
