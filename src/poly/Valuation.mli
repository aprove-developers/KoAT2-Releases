open! OurBase
(** Provides valuations which are functions mapping from variables to values. *)

open PolyTypes
(** Provides valuations which are functions mapping from variables to values. *)

(** Constructs a valuation with the corresponding indeterminate and value type *)
module MakeOverIndeterminate (I : Indeterminate) (Value : Ring) :
  Valuation with type indeterminate = I.t and type value = Value.t

(** Constructs a valuation with the variable and value type *)
module Make (Value : Ring) : sig
  include
    module type of MakeOverIndeterminate (VarIndeterminate) (Value)
    with type value = Value.t
     and type indeterminate = Var.t

  val from_native : (string * int) list -> t
  (** Creates a valuation from a string (var) to int (value) association list. *)
  end
