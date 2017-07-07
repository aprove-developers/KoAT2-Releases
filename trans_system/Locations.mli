type t = { name : string; }
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val to_string : t -> string
val of_string : string -> t
val default : t
