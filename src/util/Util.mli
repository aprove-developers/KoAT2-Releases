open Batteries

(** Returns None if one of the optionals evaluates to None.
    If all optionals evaluate to Some, it returns a list of those values. *)
val get_all : ('a Option.t) list -> ('a list) Option.t

(** Computes the maximum of the enum if non-empty, else returns None. *)
val max_option : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Option.t

(** Computes the intersection between both enums with the given equality function. *)
val intersection : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Enum.t -> 'a Enum.t 

(** Returns an enum that contains all elements that are present in the second enum but not in the first enum.  *)
val without : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Enum.t -> 'a Enum.t 
