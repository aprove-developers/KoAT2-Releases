open Batteries
open ID
open PolyTypes

(** Holds default implementations of a polynomial *)

(** Constructs a default polynomial using a list of monomials and their coefficients *)
module Make
         (Var : ID)
         (Value : Number.Numeric)
       : Polynomial with module Var = Var
                     and module Value = Value
                     and module Valuation_ = Valuation.Make(Var)(Value)
                     and module RenameMap_ = RenameMap.Make(Var)
                     and type monomial = Monomials.Make(Var)(Value).t
                     and type scaled_monomial = ScaledMonomials.Make(Var)(Value).t
