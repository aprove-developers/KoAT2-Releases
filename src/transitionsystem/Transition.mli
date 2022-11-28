open Batteries

(** A transition connects two locations and is labeled with an updated function and a guard. *)

(** Creates a transition over a given location type *)
module TransitionOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.Transition with type location = L.t
end

module TransitionSetOver(L: ProgramTypes.Location): sig
  include ProgramTypes.TransitionSet
    with type t = Set.Make(TransitionOver(L)).t
     and type elt = TransitionOver(L).t
     and type location_set = Set.Make(L).t
end

include module type of TransitionOver(Location)

(** Can be used to dump the transition into a koat file *)
val to_file_string: t -> string
