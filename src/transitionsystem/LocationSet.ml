open! OurBase
include MakeSetCreators0 (Location)

(** Returns a string representing the transition set. *)
let to_string : t -> string = Util.sequence_to_string ~f:Location.to_string % Set.to_sequence
