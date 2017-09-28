open Batteries
open PolyTypes

(** Provides default implementations of a polynomial *)

(** Constructs a default polynomial using a list of monomials and their coefficients *)
module Make
         (Value : Ring)
       : Polynomial with module Value = Value
                     and module Monomial_ = Monomials.Make(Value)
                     and type monomial = Monomials.Make(Value).t
                     
module Monadize(P : PolynomialFunctor)(R : Ring) : PolyTypes.PolyMonad    
                    with module Inner = P(R)
