open Batteries
open ID
open PolyTypes

(** Constructs a valuation with the variable and value type *)
module Make
         (Var : ID)
         (Value : Field)
       : Valuation with type var = Var.t
                    and type value = Value.t
