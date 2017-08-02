open Batteries
open ID
open PolyTypes

module Make
         (Var : ID)
         (Value : Number.Numeric)
       : Polynomial with module Var = Var
                     and module Value = Value
                     and module Valuation_ = Valuation.Make(Var)(Value)
                     and module RenameMap_ = RenameMap.Make(Var)
                     and type power = Powers.Make(Var)(Value).t
                     and type monomial = Monomials.Make(Var)(Value).t
                     and type scaled_monomial = ScaledMonomials.Make(Var)(Value).t
