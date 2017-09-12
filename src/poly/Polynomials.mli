open Batteries
open ID
open PolyTypes

(** Provides default implementations of a polynomial *)

(** Constructs a default polynomial using a list of monomials and their coefficients *)
module Make
         (Var : ID)
         (Value : Field)
       : Polynomial with module Var = Var
                     and module Value = Value
                     and module RenameMap_ = RenameMap.Make(Var)
                     and module Monomial_ = Monomials.Make(Var)(Value)
                     and type monomial = Monomials.Make(Var)(Value).t
