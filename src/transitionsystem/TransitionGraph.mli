open Batteries
(** This module represents a transition graph over simple locations *)

(** Creates a TransitionGraph over a given location type *)
module TransitionGraphOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.TransitionGraph
  with type Location.t = L.t
  and type Transition.location = L.t
  and type LocationSet.elt = L.t
  and type TransitionSet.elt = L.t * TransitionLabel.t * L.t
end

include module type of TransitionGraphOver(Location)
