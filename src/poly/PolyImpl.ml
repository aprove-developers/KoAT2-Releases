open Batteries

(** Provides all necessary types for polynomials with basic string variables and numbers represented as BigInt *)

module Valuation = Valuation.Make(ID.StringID)(PolyTypes.OurInt)
                    
module Var = ID.StringID
                       
module Monomial = Monomials.Make(ID.StringID)(PolyTypes.OurInt)
                   
module ScaledMonomial = ScaledMonomials.Make(ID.StringID)(PolyTypes.OurInt)

module Polynomial = Polynomials.Make(ID.StringID)(PolyTypes.OurInt)

module TemplatePolynomial = PolyTypes.Monadize(Polynomials.Make)(ID.StringID)(PolyTypes.OurInt)
                  
module MinMaxPolynomial = MinMaxPolynomial.Make(Polynomial)
