open OurBase

module Make (M : sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end) =
struct
  type 'a t = 'a M.t

  let ( >>= ) = M.bind
  let ( >> ) f g = f >>= fun _ -> g
  let return = M.pure
  let pure = M.pure
  let bind = M.bind

  let when_m b f =
    if b then
      f
    else
      pure ()


  let rec sequence = function
    | [] -> pure []
    | m :: ms ->
        m >>= fun v ->
        sequence ms >>= fun vs -> pure (v :: vs)


  let mapM f = sequence % List.map ~f

  let liftM2 f ma mb =
    ma >>= fun a ->
    mb >>= fun b -> pure (f a b)


  let ( let+ ) (type a b) (a : a t) (f : a -> b) = M.map f a
  let ( and+ ) (type a b) (a : a t) (b : b t) : (a * b) t = liftM2 Tuple2.make a b
  let ( let* ) = ( >>= )
  let ( and* ) = ( and+ )
end

module MakeGeneral (M : sig
  type 'a t

  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end) =
Make (struct
  include M

  let map f a = bind a (pure % f)
end)
