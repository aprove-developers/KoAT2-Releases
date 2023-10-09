open! OurBase
(** This module represents a transition graph over simple locations *)

(** Creates a TransitionGraph over given transition label, location and internal graph modules *)
module Make_
    (T : ProgramTypes.Transition)
    (G : Graph.Sig.P
           with type V.t = Location.t
            and type V.label = Location.t
            and type E.t = Location.t * T.transition_label * Location.t
            and type E.label = T.transition_label) : sig
  include
    ProgramTypes.TransitionGraph
      with type transition_label = T.transition_label
       and type transition_label_comparator_witness = T.transition_label_comparator_witness
       and type transition = T.t
       and type t = G.t
end

(** Creates a TransitionGraph over given transition label and location modules *)
module TransitionGraph : sig
  include
    ProgramTypes.TransitionGraph
      with type transition_label = TransitionLabel_.t
       and type transition_label_comparator_witness = TransitionLabel_.comparator_witness
       and type transition = Transition_.Make(TransitionLabel_).t
end

include module type of TransitionGraph with type t = TransitionGraph.t
