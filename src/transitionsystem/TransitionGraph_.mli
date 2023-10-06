open! OurBase
(** This module represents a transition graph over simple locations *)

(** Creates a TransitionGraph over given transition label, location and internal graph modules *)
module Make_
    (T : ProgramTypes.Transition)
    (L : ProgramTypes.Location
           with type t = T.location
            and type comparator_witness = T.location_comparator_witness)
    (G : Graph.Sig.P
           with type V.t = L.t
            and type V.label = L.t
            and type E.t = L.t * T.transition_label * L.t
            and type E.label = T.transition_label) : sig
  include
    ProgramTypes.TransitionGraph
      with type location = L.t
       and type location_comparator_witness = L.comparator_witness
       and type transition_label = T.transition_label
       and type transition_label_comparator_witness = T.transition_label_comparator_witness
       and type transition = T.t
       and type t = G.t

  val add_invariant : unit
end

(** Creates a TransitionGraph over given transition label and location modules *)
module TransitionGraphOverLocation (L : ProgramTypes.Location) : sig
  include
    ProgramTypes.TransitionGraph
      with type location = L.t
       and type location_comparator_witness = L.comparator_witness
       and type transition_label = TransitionLabel_.t
       and type transition_label_comparator_witness = TransitionLabel_.comparator_witness
       and type transition = Transition_.Make(TransitionLabel_)(L).t
end

include module type of TransitionGraphOverLocation (Location)
