open Batteries
open PolyTypes

(** Constructs a valuation with the variable and value type *)
module Make
         (Value : Ring)
       : Valuation with type value = Value.t
