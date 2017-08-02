open Batteries

module Make
         (Var : PolyTypes.ID)
         (Value : Number.Numeric)
       : PolyTypes.MinMaxPolynomial with module Var = Var
                                     and module Value = Value
                                     and module Valuation_ = Valuation.Make(Var)(Value)
                                     and module RenameMap_ = RenameMap.Make(Var)
                                     and module Polynomial_ = Polynomials.Make(Var)(Value)
