open Batteries

(** Provides all necessary types for polynomials with basic string variables and numbers represented as BigInt *)

module Valuation = Valuation.Make(ID.StringID)(PolyTypes.OurInt)
                    
module Var = ID.StringID
                       
module Monomial = Monomials.Make(ID.StringID)(PolyTypes.OurInt)
                   
module ScaledMonomial = ScaledMonomials.Make(ID.StringID)(PolyTypes.OurInt)

module Polynomial = Polynomials.Make(ID.StringID)(PolyTypes.OurInt)

(** TODO Put into appropiate place *)
(** TODO Maybe make generic, if possible *)
module TemplatePolynomial = struct
  module PolyOver = Polynomials.Make(ID.StringID)
  module Inner = PolyOver(PolyTypes.OurInt)
  module Outer = PolyOver(Inner)
  include Outer
  (** Transforms the template polynomial such that all inner values get lifted to the outer polynomial. *)
  (** Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)
  let flatten (templatepoly : t): Inner.t =
    Outer.fold ~const:identity ~var:Inner.from_var ~neg:Inner.neg ~plus:Inner.add ~times:Inner.mul ~pow:Inner.pow templatepoly
end
                  
module MinMaxPolynomial = MinMaxPolynomial.Make(ID.StringID)(PolyTypes.OurInt)
