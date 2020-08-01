open Batteries

module Make :
  functor ( M :
    sig
      type 'a t
      val pure: 'a -> 'a t
      val bind: 'a t -> ('a -> 'b t) -> 'b t
    end) ->
  sig
    type 'a t = 'a M.t
    val pure: 'a -> 'a t
    val return: 'a -> 'a t

    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    val (>>): 'a t -> 'b t -> 'b t

    val when_m: bool -> unit t -> unit t
  end