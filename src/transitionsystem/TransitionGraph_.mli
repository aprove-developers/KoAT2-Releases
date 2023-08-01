open OurBase
(** This module represents a transition graph over simple locations *)

(** Creates a TransitionGraph over given transition label, location and internal graph modules *)
module Make_(T: ProgramTypes.Transition)
            (L: ProgramTypes.Location with type t = T.location)
            (G: Graph.Sig.P with type V.t = L.t
                             and type V.label = L.t
                             and type E.t = L.t * T.transition_label * L.t
                             and type E.label = T.transition_label): sig
  include ProgramTypes.TransitionGraph
    with type location = L.t
     and type location_set = Location.LocationSetOver(L).t
     and type transition_label = T.transition_label
     and type transition = T.t
     and type transition_set = (T.t, T.comparator_witness) Set.t
     and type t = G.t
end

(** Creates a TransitionGraph over given transition label and location modules *)
module TransitionGraphOverLocation(L: ProgramTypes.Location) : sig
  include ProgramTypes.TransitionGraph
    with type location = L.t
     and type location_set = Location.LocationSetOver(L).t
     and type transition_label = TransitionLabel_.t
     and type transition = Transition_.TransitionOver(TransitionLabel_)(L).t
     and type transition_set = (Transition_.TransitionOver(TransitionLabel_)(L).t, Transition_.TransitionOver(TransitionLabel_)(L).comparator_witness) Set.t
end

include module type of TransitionGraphOverLocation(Location)
