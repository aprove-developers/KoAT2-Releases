open Batteries

(** Provides default implementations of an ID *)
   
(** An ID is a unique identifier for the elements of an arbitrary set (of variables) *)
type t =
  | Var of String.t
  | Helper of int [@@deriving eq, ord]

val compare : t -> t -> int
val (=~=) : t -> t -> bool
val of_string : string -> t
val to_string : t -> string
(** Returns a not yet used id, which is guaranteed to be distinct from any yet existing ids. *)
val fresh_id : unit -> t
    (** Returns a bunch of fresh ids. *)
val fresh_ids : int -> t Enum.t
val fresh_id_list : int -> t list
val is_helper : t -> bool
val mk_helper : int -> t
