open Batteries

(** Provides default implementations of an ID *)

(** Finite type for giving a helper a type *)
type sort =
  | Real
  | Int [@@deriving eq, ord]

(**  Indicates how a Variable can be substituded in a Bound/Polynomial *)
type substitution_kind =
  NonProbabilistic
  | Probabilistic [@@deriving eq, ord]

(** An ID is a unique identifier for the elements of an arbitrary set (of variables) *)
type t =
  | Var of substitution_kind*String.t
  (**Helpers are fresh variables generated via the computation. They represent a real or an integer value.*)
  | Helper of sort*int
  | Argument of int [@@deriving eq, ord]


val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val (=~=) : t -> t -> bool
val of_string : string -> t
val to_string : t -> string
(** Returns a not yet used id, which is guaranteed to be distinct from any yet existing ids. *)
val fresh_id : sort -> unit -> t
    (** Returns a bunch of fresh ids. *)
val fresh_ids : sort -> int -> t Enum.t
val fresh_id_list : sort -> int -> t list
val fresh_arg_list : int -> t list
val is_helper : t -> bool
val mk_helper : sort -> int -> t
val is_real : t -> bool
val get_substitution_kind : t -> substitution_kind
val set_substitution_kind : substitution_kind -> t -> t
