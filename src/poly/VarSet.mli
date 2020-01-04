(** Provides implemenation variable sets. *)
open Batteries

include module type of Set.Make(Var)
   
(** TODO doc *)
val map_to_set : (elt -> 'b) -> t -> 'b Set.t

(** TODO doc *)
val map_to_list : (elt -> 'b) -> t -> 'b list

(** TODO doc *)
val map_to_array : (elt -> 'b) -> t -> 'b array

(** Returns a string representing a set of variables. *)
val to_string : t -> string

(** Creates a set of variables from a list of strings. *)
val of_string_list : string list -> t

(** TODO doc *)
val powerset : t -> t Enum.t

(** TODO doc *)
val combinations : int -> t -> t list

(** TODO doc *)
val sorted_combinations : int -> t -> t Enum.t