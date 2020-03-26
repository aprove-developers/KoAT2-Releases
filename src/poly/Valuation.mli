(** Provides valuations which are functions mapping from variables to values. *)
open Batteries
open PolyTypes
(** Provides valuations which are functions mapping from variables to values. *)

(** Constructs a valuation with the variable and value type *)
module Make
         (Value : Ring)
       : Valuation with type value = Value.t
