open Batteries

(** Provides all necessary types for polynomials with basic string variables and numbers represented as BigInt *)

module Valuation = Valuation.Make(PolyTypes.OurInt)
                    
module Monomial = Monomials.Make(PolyTypes.OurInt)
                   
module ScaledMonomial = ScaledMonomials.Make(PolyTypes.OurInt)

module Polynomial = Polynomials.Make(PolyTypes.OurInt)

module TemplatePolynomial = Polynomials.Monadize(Polynomials.Make)(PolyTypes.OurInt)
                  
module MinMaxPolynomial = MinMaxPolynomial.Make(Polynomial)
