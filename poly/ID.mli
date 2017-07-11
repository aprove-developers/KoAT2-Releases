open Batteries

module type ID =
  sig
    type t
    val (==) : t -> t -> bool
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

module StringID : ID

module PrePostID : ID
