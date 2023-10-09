open! OurBase

module Inner = struct
  type t = string [@@deriving eq, ord, sexp]

  let to_string l = l
  let hash l = Hashtbl.hash l
  let of_string name = name
end

include Inner
include Comparator.Make (Inner)
