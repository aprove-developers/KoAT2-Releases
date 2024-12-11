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

val uniform : 'a list -> 'a t
(** Creates a uniform distribution from a list of values.
    Raises an error if the list is empty. *)

val uniform_from_to : OurInt.t -> OurInt.t -> OurInt.t t
(** [uniform_from_to a b] creates a uniform distribution over the range of integers 
    from [a] to [b] (inclusive). Raises an error if [a > b]. *)

val geo : OurRational.t -> OurInt.t t
(** [geo p] creates a geometric distribution with success probability [p]. *)

val binomial : OurInt.t -> OurRational.t -> OurInt.t t
(** [binomial n p] creates a binomial distribution with [n] trials and success probability [p]. *)

val hyper_geometric : OurInt.t -> OurInt.t -> OurInt.t -> OurInt.t t
(** [hyper_geometric bigN k n] creates a hypergeometric distribution. *)

val to_list : 'a t -> ('a * Q.t) list
(** [to_list dist] converts [dist] into a list of pairs, where each pair represents a leaf in the distribution tree with its value and the probability of reaching that leaf. *)

val to_grouped_list : 'a t -> cmp:('a -> 'a -> int) -> ('a * Q.t) list
(** Similar to [to_list], but groups tuples based on the comparison function [cmp].
    Combines probabilities for equivalent values, so each tuple [(a, p)] associates the value [a] with the total probability of reaching a leaf with value [a]. *)

val to_string : f:('a -> string) -> 'a t -> string

module Monad : MonadType.Monad with type 'a t := 'a t
