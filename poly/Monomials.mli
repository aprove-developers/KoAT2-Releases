open Batteries

(** Holds default implementations of a monomial *)

(** Constructs a default monomial using a list of pairs of variables and their exponents *)
module Make
         (Var : PolyTypes.ID)
         (Value : Number.Numeric)
       : PolyTypes.Monomial with module Var = Var
                             and module Value = Value
                             and module Valuation_ = Valuation.Make(Var)(Value)
                             and module RenameMap_ = RenameMap.Make(Var)
