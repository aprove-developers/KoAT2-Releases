open Batteries 

module LocationSetOver(L: ProgramTypes.Location) = struct
  include Set.Make(L)

  let to_string = Util.enum_to_string L.to_string % enum
end

type t = string [@@deriving eq, ord]

let to_string l = l

let hash l = Hashtbl.hash l

let of_string name = name
