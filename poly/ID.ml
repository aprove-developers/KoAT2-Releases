open Batteries

module type ID =
  sig
    type t
    val (==) : t -> t -> bool
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

module StringID =
  struct
    type t = String.t
    let (==) = String.equal
    let of_string str = str
    let to_string var = var
    let compare = String.compare
  end
