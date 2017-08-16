open Batteries

(** Provides all necessary types for polynomials with basic string variables and numbers represented as BigInt *)
   
module Valuation = Valuation.Make(ID.StringID)(Number.MakeNumeric(Big_int))
                    
module Var = ID.StringID
                       
module Monomial = Monomials.Make(ID.StringID)(Number.MakeNumeric(Big_int))
                   
module ScaledMonomial = ScaledMonomials.Make(ID.StringID)(Number.MakeNumeric(Big_int))
                         
module Polynomial = Polynomials.Make(ID.StringID)(Number.MakeNumeric(Big_int))

module MinMaxPolynomial = MinMaxPolynomial.Make(ID.StringID)(Number.MakeNumeric(Big_int))
