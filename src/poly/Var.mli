open OurBase

(** Provides default implementations of variables. *)

(** Provides default implementations of an ID. *)

(** Finite type for giving a helper a type. *)
type sort =
  | Real
  | Int [@@deriving eq, ord]

(** An ID is a unique identifier for the elements of an arbitrary set (of variables). *)
type t =
  | Var of String.t
  (**Helpers are fresh variables generated via the computation. They represent a real or an integer value.*)
  | Helper of sort*int
  | Argument of int

include Comparator.S with type t := t
include Sexpable.S with type t := t

(** TODO doc *)
val equal : t -> t -> bool

(** TODO doc *)
val compare : t -> t -> int

(** TODO doc *)
val hash : t -> int

(** TODO doc *)
val (=~=) : t -> t -> bool

(** Creates a variable from a string. *)
val of_string : string -> t

 (** Returns a string representing the variable([Temp_Int_] for integer variables, [Temp_Real_] for real variables and [Arg_ ] for argument variables). Parameter {i to_file} is used to get a representation with less special characters. *)
val to_string : ?pretty:bool -> ?to_file:bool -> t -> Batteries.String.t

(** An (infinite) list of all possible argument variables *)
val args: t Sequence.t

(** Returns a not yet used id, which is guaranteed to be distinct from any yet existing ids. *)
val fresh_id : sort -> unit -> t

(** Returns a bunch of fresh ids. *)
val fresh_id_list : sort -> int -> t list

(** Returns true if variable has type [Helper]. *)
val is_helper : t -> bool

(** Creates a helper variable of type [sort] and a given id. *)
val mk_helper : sort -> int -> t

(** Returns true if variable is ranged over the integers. *)
val is_integral: t -> bool

(** Returns true if variable is ranged over real numbers. *)
val is_real : t -> bool
