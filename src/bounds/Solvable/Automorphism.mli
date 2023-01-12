open Batteries
open Polynomials

module Automorphism :
sig
    type t

    val to_string: t -> string

    val identity : t
end
