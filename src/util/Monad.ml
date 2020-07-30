module Make
  (M :
    sig
      type 'a t
      val pure : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end) =

  struct
    type 'a t = 'a M.t

    let (>>=) = M.bind
    let (>>) f g = f >>= fun _ -> g

    let return = M.pure
    let pure = M.pure
    let bind = M.bind
  end