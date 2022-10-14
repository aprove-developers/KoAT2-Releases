(** A transition connects two locations and is labeled with an updated function and a guard. *)

(** Creates a transition over a given location type *)
module TransitionOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.Transition
  with module Location = L
end

include module type of TransitionOver(Location)
