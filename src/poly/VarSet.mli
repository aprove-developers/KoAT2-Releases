open! OurBase
(** Provides implemenation variable sets. *)

include module type of MakeSetCreators0 (Var)

val equal : t -> t -> bool

val map_to_list : (elt -> 'b) -> t -> 'b list
(** TODO doc *)

val map_to_array : (elt -> 'b) -> t -> 'b array
(** TODO doc *)

val to_string : ?pretty:bool -> t -> string
(** Returns a string representing a set of variables. *)

val of_string_list : string list -> t
(** Creates a set of variables from a list of strings. *)

(* (\** TODO doc *\) *)
(* val combinations : int -> t -> t list *)
