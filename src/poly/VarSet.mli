(** Provides implemenation variable sets. *)
open OurBase

include module type of MakeSetCreators0(Var)

val equal: t -> t -> bool

(** TODO doc *)
val map_to_list : (elt -> 'b) -> t -> 'b list

(** TODO doc *)
val map_to_array : (elt -> 'b) -> t -> 'b array

(** Returns a string representing a set of variables. *)
val to_string : ?pretty:bool -> t -> string

(** Creates a set of variables from a list of strings. *)
val of_string_list : string list -> t

(** TODO doc *)
val combinations : int -> t -> t list
