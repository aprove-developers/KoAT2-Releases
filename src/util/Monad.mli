open Batteries

module Make
  (M :
    sig
      type 'a t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val pure: 'a -> 'a t
      val bind: 'a t -> ('a -> 'b t) -> 'b t
    end): MonadType.Monad with type 'a t = 'a M.t

(** This chooses the default implementation [ map f a = bind a (pure % f) ] and might hence be less efficient *)
module MakeGeneral
  (M :
    sig
      type 'a t
      val pure: 'a -> 'a t
      val bind: 'a t -> ('a -> 'b t) -> 'b t
    end): MonadType.Monad with type 'a t = 'a M.t
