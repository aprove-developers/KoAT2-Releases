open Batteries

module MakeMinMaxPolynomial
         (Var : PolyTypes.ID)
         (Value : Number.Numeric)
       : PolyTypes.MinMaxPolynomial with module Var = Var
                                     and module Value = Value
                                     and module Valuation_ = Valuation.MakeValuation(Var)(Value)
                                     and module RenameMap_ = RenameMap.MakeRenameMap(Var)
                                     and module Polynomial_ = Polynomials.MakePolynomial(Var)(Value)
