type t = string [@@deriving eq, ord]

let to_string l = l

let hash l = Hashtbl.hash l

let of_string name = name
