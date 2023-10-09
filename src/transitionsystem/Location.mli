open! OurBase
(** The simples location possible. Identified just by it's name *)

type t
(** Type of location, we use strings. *)

val equal : t -> t -> bool
val compare : t -> t -> int

val hash : t -> int
(** Generates a hash value for a location.*)

val to_string : t -> string
(** Returns a string representing a location. *)

val of_string : string -> t
(** Creates a location from a given name. *)

include Comparator.S with type t := t
include Sexpable.S with type t := t
