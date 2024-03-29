open Batteries
(** Util *)

val get_all : 'a Option.t list -> 'a list Option.t
(** Returns None if one of the optionals evaluates to None.
    If all optionals evaluate to Some, it returns a list of those values. *)

val group : ('a -> 'a -> bool) -> 'a list -> 'a list list

val max_option : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Option.t
(** Computes the maximum of the enum if non-empty, else returns None. *)

val min_option : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Option.t
(** Computes the maximum of the enum if non-empty, else returns None. *)

val intersection : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Enum.t -> 'a Enum.t
(** Computes the intersection between both enums with the given equality function. *)

val without : ('a -> 'a -> bool) -> 'a Enum.t -> 'a Enum.t -> 'a Enum.t
(** Returns an enum that contains all elements that are present in the second enum but not in the first enum.  *)

val option_to_string : ('a -> string) -> 'a Option.t -> string
val enum_to_string : ('a -> string) -> 'a Enum.t -> string
val sequence_to_string : 'a OurBase.Sequence.t -> f:('a -> string) -> string
val powerset : 'a Set.t -> 'a Set.t Enum.t
val find_map : ('a -> 'b Option.t) -> 'a Enum.t -> 'b Option.t
val memoize : ('a, 'b) Hashtbl.t -> extractor:('c -> 'a) -> ('c -> 'b) -> 'c -> 'b
val memoize_base_hashtbl : ('a, 'b) Base.Hashtbl.t -> extractor:('c -> 'a) -> ('c -> 'b) -> 'c -> 'b

val hash : string -> int
(**  Generates a hash interger value of a given string. *)

val find_fixpoint : ('a -> 'a MaybeChanged.t) -> 'a -> 'a
val find_fixpoint_mc : ('a -> 'a MaybeChanged.t) -> 'a -> 'a MaybeChanged.t

(* Execute a function and measure its execution time. The time used by the function is then printed to stdout *)
val measure_execution_time : ?methodname:string -> (unit -> 'a) -> 'a

(* Returns a function to measure the execution time of an evaluation, and a function to get the total amount of measured seconds *)
val measure_total_execution_time : unit -> ((unit -> 'a) -> 'a) * (string -> unit)
val natural_to_subscript : int -> string
val natural_to_superscript : int -> string
val read_from_channel : BatInnerIO.input -> string
val read_process : string -> string
val iterate_n_times : ('a -> 'a) -> int -> 'a -> 'a
