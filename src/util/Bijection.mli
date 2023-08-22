open! OurBase

(** Bijection internally implemented using 2 seperate Maps *)

type ('a, 'b, 'a_cmp, 'b_cmp) t

val of_sequence :
  ('a, 'a_cmp) Comparator.Module.t ->
  ('b, 'b_cmp) Comparator.Module.t ->
  ('a * 'b) Sequence.t ->
  ('a, 'b, 'a_cmp, 'b_cmp) t

val find_exn : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'a -> 'b

val finda_exn : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'a -> 'b
(** [finda_exn] is the same as [find_exn] *)

val findb_exn : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'b -> 'a
val find : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'a -> 'b Option.t

val finda : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'a -> 'b Option.t
(** [finda] is ('a,'b,'a_cmp,'b_cmp) the same as [find] *)

val findb : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'b -> 'a Option.t
val to_sequence : ('a, 'b, 'a_cmp, 'b_cmp) t -> ('a * 'b) Sequence.t
val to_sequence_rev : ('a, 'b, 'a_cmp, 'b_cmp) t -> ('b * 'a) Sequence.t
val amap : ('a, 'b, 'a_cmp, 'b_cmp) t -> ('a, 'b, 'a_cmp) Map.t
val bmap : ('a, 'b, 'a_cmp, 'b_cmp) t -> ('b, 'a, 'b_cmp) Map.t
val keys : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'a List.t
val values : ('a, 'b, 'a_cmp, 'b_cmp) t -> 'b List.t

val remove : 'a -> ('a, 'b, 'a_cmp, 'b_cmp) t -> ('a, 'b, 'a_cmp, 'b_cmp) t
(** removes the mapping *)

val removea : 'a -> ('a, 'b, 'a_cmp, 'b_cmp) t -> ('a, 'b, 'a_cmp, 'b_cmp) t
(** [removea] is the same as [remove] *)

val removeb : 'b -> ('a, 'b, 'a_cmp, 'b_cmp) t -> ('a, 'b, 'a_cmp, 'b_cmp) t

val add : 'a -> 'b -> ('a, 'b, 'a_cmp, 'b_cmp) t -> ('a, 'b, 'a_cmp, 'b_cmp) t
(** Throws an exception if the key is already contained in the mapping *)

(** Provides helpful [of_x] functions *)
module Make (A : Comparator.S) (B : Comparator.S) : sig
  type ('a, 'b, 'a_cmp, 'b_cmp) outer_t = ('a, 'b, 'a_cmp, 'b_cmp) t
  type t = (A.t, B.t, A.comparator_witness, B.comparator_witness) outer_t

  val of_sequence : (A.t * B.t) Sequence.t -> t
  (** If the sequence does not describe a bijection, an exception is thrown *)

  val of_sequence_rev : (B.t * A.t) Sequence.t -> t
end
