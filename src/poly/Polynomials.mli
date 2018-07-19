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

  val max_of_occurring_constants : t -> OurInt.t

end
     
module ParameterPolynomial :
sig
  include module type of PolynomialOver(PolynomialOver(OurInt))

  val eval_coefficients : (Var.t -> OurInt.t) -> PolynomialOver(PolynomialOver(OurInt)).t -> PolynomialOver(OurInt).t

  val flatten : t -> PolynomialOver(OurInt).t
  
  val of_polynomial : PolynomialOver(OurInt).t -> t
end

module RealPolynomial :
sig
  include module type of PolynomialOver(OurFloat)

  val separate_by_sign : t -> (t * t)

  val max_of_occurring_constants : t -> OurFloat.t
  val of_intpoly : Polynomial.t -> t
end
     
module RealParameterPolynomial :
sig
  include module type of PolynomialOver(PolynomialOver(OurFloat))

  val eval_coefficients : (Var.t -> OurFloat.t) -> PolynomialOver(PolynomialOver(OurFloat)).t -> PolynomialOver(OurFloat).t

  val flatten : t -> PolynomialOver(OurFloat).t
  
  val of_polynomial : PolynomialOver(OurFloat).t -> t

  val of_int_parapoly : PolynomialOver(PolynomialOver(OurInt)).t -> t
end
