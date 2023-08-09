include Monad.Make (struct
  type 'a t = 'a List.t

  let map = List.map
  let pure (e : 'a) : 'a list = [ e ]
  let bind (xs : 'a list) (f : 'a -> 'b list) : 'b list = List.map f xs |> List.flatten
end)
