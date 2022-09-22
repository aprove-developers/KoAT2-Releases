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

include ProgramTypes.TransitionGraph
  with type Location.t = Location.t
  and type Transition.location = Location.t
  and type LocationSet.elt = Location.t
  and type TransitionSet.elt = Location.t * TransitionLabel.t * Location.t
