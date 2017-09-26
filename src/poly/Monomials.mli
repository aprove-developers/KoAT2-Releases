open Batteries

(** Provides default implementations of a monomial *)

(** Constructs a default monomial using a list of pairs of variables and their exponents *)
module Make
         (Value : PolyTypes.Ring)
       : PolyTypes.Monomial with module Value = Value
                             and module Valuation_ = Valuation.Make(Value)
                             and module RenameMap_ = RenameMap.Make
