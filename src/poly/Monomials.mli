(** Provides default implementation of a monomial. *)
open Batteries
(** Provides default implementation of a monomial, i.e.,  a finite product of powers without a constant (e.g.: xy^2, y but not 5xy^2 + 7). *)

(** Constructs a default monomial using a list of pairs of variables and their exponents. *)
module Make
         (Value : PolyTypes.Ring)
       : PolyTypes.Monomial with type value = Value.t
                             and type valuation = Valuation.Make(Value).t
