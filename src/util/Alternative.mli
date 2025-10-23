open! OurBase

module Make (A : sig
  include MonadType.Monad

  val empty : 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
end) : sig
  include MonadType.Monad with type 'a t = 'a A.t

  val empty : 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val some : 'a t -> 'a List.t t
  val many : 'a t -> 'a List.t t
end
