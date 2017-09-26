open Batteries

(** Provides default implementations of a polynomial with min and max functions *)

(** Constructs a default MinMaxPolynomial based on a polynomial extending it with an algebraic data type *)
module Make
         (P : PolyTypes.Polynomial)
       : PolyTypes.MinMaxPolynomial with module Value = P.Value
                                     and module Valuation_ = P.Valuation_
                                     and module Polynomial_ = P
