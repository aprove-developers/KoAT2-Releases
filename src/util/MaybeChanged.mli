open! OurBase

(** A type that is wrapped by [MaybeChanged.t] represents a value that might change during a computation.
    Once it has changed it will be forever marked as changed, but can also be further be manipulated. *)

type status = Changed | Same
type 'a t

val return : 'a -> 'a t
(** Sets the status to [Same]*)

val same : 'a -> 'a t
(** Same as return *)

val changed : 'a -> 'a t
(** Creates a MaybeChanged and sets the status to [Changed] *)

val status : 'a t -> status
val pure : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** inline version of [bind]*)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val unpack : 'b t -> 'b
val has_changed : 'a t -> bool
val if_changed : ('a -> 'a) -> 'a t -> 'a t
val fold_sequence : f:('a -> 'b -> 'a t) -> init:'a -> 'b Sequence.t -> 'a t
val fold : ('a -> 'b -> 'a t) -> 'a -> 'b List.t -> 'a t

module Monad : MonadType.Monad with type 'a t := 'a t
