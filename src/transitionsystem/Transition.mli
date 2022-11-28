open Batteries

(** A transition connects two locations and is labeled with an updated function and a guard. *)

(** Creates a transition over a given location type *)
module TransitionOver(TL: ProgramTypes.TransitionLabel)(L: ProgramTypes.Location) : sig
  include ProgramTypes.Transition
    with type location = L.t
     and type transition_label = TL.t
end

module TransitionSetOver(T: ProgramTypes.Transition)(L: ProgramTypes.Location with type t = T.location): sig
  include ProgramTypes.TransitionSet
    with type t = Set.Make(T).t
     and type elt = T.t
     and type location_set = Set.Make(L).t
end


include module type of TransitionOver(TransitionLabel)(Location)

(** Can be used to dump the transition into a koat file *)
val to_file_string: t -> string
