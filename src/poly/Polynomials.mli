open Batteries
open PolyTypes

(** Provides default implementations of a polynomial *)

(** Constructs a default polynomial using a list of monomials and their coefficients *)
module PolynomialOver
         (Value : Ring)
       : Polynomial with type value = Value.t
                     and type valuation = Valuation.Make(Value).t
                     and type monomial = Monomials.Make(Value).t
                    
module Polynomial :
sig
  include module type of PolynomialOver(OurInt)

  val separate_by_sign : t -> (t * t)
end
     
module ParameterPolynomial :
sig
  include module type of PolynomialOver(PolynomialOver(OurInt))

  val flatten : t -> PolynomialOver(OurInt).t
  
  val of_polynomial : PolynomialOver(OurInt).t -> t
end
