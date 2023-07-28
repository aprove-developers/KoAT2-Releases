(** Util *)
open Batteries

(** Returns None if one of the optionals evaluates to None.
    If all optionals evaluate to Some, it returns a list of those values. *)
val get_all : ('a Option.t) list -> ('a list) Option.t

val group : ('a -> 'a -> bool) -> 'a list -> 'a list list

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

val sequence_to_string: 'a OurBase.Sequence.t -> f:('a -> string) -> string

val powerset : 'a Set.t -> ('a Set.t) Enum.t

val find_map : ('a -> 'b Option.t) -> 'a Enum.t -> 'b Option.t

val memoize : ('a,'b) Hashtbl.t -> extractor:('c -> 'a) -> ('c -> 'b) -> 'c -> 'b

val memoize_base_hashtbl : ('a,'b) Base.Hashtbl.t -> extractor:('c -> 'a) -> ('c -> 'b) -> 'c -> 'b

(**  Generates a hash interger value of a given string. *)
val hash: string -> int

val cat_maybes : 'a option list -> 'a list

val cat_maybes_sequence : 'a option OurBase.Sequence.t -> 'a OurBase.Sequence.t

val cat_maybes_enum : 'a option Enum.t -> 'a Enum.t

val map_maybe : ('a -> 'b) -> 'a option list -> 'b list

val find_fixpoint : ('a -> MaybeChanged.status * 'a) -> 'a -> 'a

(* Execute a function and measure its execution time. The time used by the function is then printed to stdout *)
val measure_execution_time : ?methodname:string -> (unit -> 'a) -> 'a

(* Returns a function to measure the execution time of an evaluation, and a function to get the total amount of measured seconds *)
val measure_total_execution_time : unit -> ((unit -> 'a) -> 'a) * (string -> unit)

val natural_to_subscript : int -> string

val natural_to_superscript : int -> string

val read_from_channel: BatInnerIO.input -> string

val read_process: string -> string

val iterate_n_times: ('a -> 'a) -> int -> 'a -> 'a

(** Reasoning about type equalities *)
module TypeEq: sig
  type (_,_) t = | Refl: ('a,'a) t

  (** transitivity *)
  val trans: ('a,'b) t -> ('b,'c) t -> ('a,'c) t

  (** symmetry *)
  val sym: ('a,'b) t -> ('b,'a) t

  val coerce: ('a,'b) t -> 'a -> 'b
end
