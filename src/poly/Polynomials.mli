open Batteries
open PolyTypes

(** Provides default implementations of a polynomial *)

(** Constructs a default polynomial using a list of monomials and their coefficients *)
module Make
         (Value : Ring)
       : Polynomial with type value = Value.t
                     and module Monomial_ = Monomials.Make(Value)
                     and type valuation = Valuation.Make(Value).t
                     and type monomial = Monomials.Make(Value).t
                    
