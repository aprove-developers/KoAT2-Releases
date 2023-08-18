open OurBase

module LocationSetOver (L : ProgramTypes.Location) = struct
  include Set
  include MakeSetCreators0 (L)

  let to_string : t -> string = Util.sequence_to_string ~f:L.to_string % Set.to_sequence
end

module Inner = struct
  type t = string [@@deriving eq, ord, sexp]

  let to_string l = l
  let hash l = Hashtbl.hash l
  let of_string name = name
end

include Inner
include Comparator.Make (Inner)
