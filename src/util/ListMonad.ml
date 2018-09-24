let pure (e : 'a) : 'a list = 
  [e]

let (>>=) (xs : 'a list) (f : 'a -> 'b list) : ('b list) = 
  List.map f xs
  |> List.flatten
