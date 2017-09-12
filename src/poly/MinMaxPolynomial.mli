open Batteries

(** Provides default implementations of a polynomial with min and max functions *)

(** Constructs a default MinMaxPolynomial based on a polynomial extending it with an algebraic data type *)
module Make
         (Var : PolyTypes.ID)
         (Value : PolyTypes.Field)
       : PolyTypes.MinMaxPolynomial with module Var = Var
                                     and module Value = Value
                                     and module Valuation_ = Valuation.Make(Var)(Value)
                                     and module RenameMap_ = RenameMap.Make(Var)
                                     and module Polynomial_ = Polynomials.Make(Var)(Value)
