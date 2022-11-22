open Batteries
(** This module represents a transition graph over simple locations *)

(** Creates a TransitionGraph over a given location type *)
module TransitionGraphOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.TransitionGraph
  with type location = L.t
  and  type location_set = Set.Make(L).t
  and  type transition = Transition.TransitionOver(L).t
  and  type transition_set = Set.Make(Transition.TransitionOver(L)).t
end

include module type of TransitionGraphOver(Location)
