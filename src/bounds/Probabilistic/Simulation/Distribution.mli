open OurBase

type 'a t

val pure : 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val choice : OurRational.t -> (unit -> 'a t) -> (unit -> 'a t) -> 'a t

val of_tuples : (OurRational.t * 'a) Sequence.t -> 'a t
(** Creates a new distribution from a list of tuples, where each tuple (p, x) represents:
  - p: the probability of reaching x,
    - x: the value stored in the corresponding leaf.

    The probabilities must sum to 1. *)

val iter_n : int -> f:(OurRational.t -> 'a -> unit) -> 'a t -> unit
val to_string : f:('a -> string) -> 'a t -> string

module Monad : MonadType.Monad with type 'a t := 'a t
