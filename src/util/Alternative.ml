open! OurBase

module Make (A : sig
  include MonadType.Monad

  val empty : 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
end) =
struct
  include A

  let rec some (fa : 'a t) : 'a List.t t =
    let* a = fa in
    let* rem = many fa in
    pure (a :: rem)


  and many (fa : 'a t) : 'a List.t t = some fa <|> pure []
end
