open Batteries

module Valuation = Valuation.MakeValuation(ID.StringID)(Number.MakeNumeric(Big_int))
                    
module VariableTerm = Variables.MakeVariableTerm(ID.StringID)(Number.MakeNumeric(Big_int))
                       
module Power = Powers.MakePower(ID.StringID)(Number.MakeNumeric(Big_int))
                
module Monomial = Monomials.MakeMonomial(ID.StringID)(Number.MakeNumeric(Big_int))
                   
module ScaledMonomial = ScaledMonomials.MakeScaledMonomial(ID.StringID)(Number.MakeNumeric(Big_int))
                         
module Polynomial = Polynomials.MakePolynomial(ID.StringID)(Number.MakeNumeric(Big_int))
