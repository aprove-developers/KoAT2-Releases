type t = { name : string; vars : Variables.t list; }
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
