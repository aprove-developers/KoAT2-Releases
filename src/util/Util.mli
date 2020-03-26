(** Util *)
open Batteries

(** Returns None if one of the optionals evaluates to None.
    If all optionals evaluate to Some, it returns a list of those values. *)
val get_all : ('a Option.t) list -> ('a list) Option.t

(** Computes the maximum of the enum if non-empty, else returns None. *)
val max_option : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Option.t

(** Computes the maximum of the enum if non-empty, else returns None. *)
val min_option : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Option.t

(** Computes the intersection between both enums with the given equality function. *)
val intersection : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Enum.t -> 'a Enum.t 

(** Returns an enum that contains all elements that are present in the second enum but not in the first enum.  *)
val without : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Enum.t -> 'a Enum.t 

val option_to_string : ('a -> string) -> 'a Option.t -> string
  
val enum_to_string : ('a -> string) -> 'a Enum.t -> string

val powerset : 'a Set.t -> ('a Set.t) Enum.t

val find_map : ('a -> 'b Option.t) -> 'a Enum.t -> 'b Option.t

val memoize : extractor:('a -> 'c) -> ('a -> 'b) -> 'a -> 'b  

(**  Generates a hash interger value of a given string. *)
val hash: string -> int