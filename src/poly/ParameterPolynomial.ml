open Batteries

module Outer = Polynomials.Make(Polynomials.Make(OurInt))
module Inner = Polynomials.Make(OurInt)

include Outer

(** Transforms the template polynomial such that all inner values get lifted to the outer polynomial. *)
(** Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)
let flatten (templatepoly : Outer.t): Inner.t =
  Outer.fold ~const:identity ~var:Inner.from_var ~neg:Inner.neg ~plus:Inner.add ~times:Inner.mul ~pow:Inner.pow templatepoly

