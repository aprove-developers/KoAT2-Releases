(** A transition connects two locations and is labeled with an updated function and a guard. *)

(** Creates a transition over a given location type *)
module TransitionOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.Transition
  with type location = L.t
end

include ProgramTypes.Transition
  with type location = Location.t
