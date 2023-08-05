open OurBase

(** A transition connects two locations and is labeled with an updated function and a guard. *)

(** Creates a transition over a given location type *)
module TransitionOver(TL: ProgramTypes.TransitionLabel)(L: ProgramTypes.Location) : sig
  include ProgramTypes.Transition
    with type location = L.t
     and type location_comparator_witness = L.comparator_witness
     and type transition_label = TL.t
     and type transition_label_comparator_witness = TL.comparator_witness
end

module TransitionSetOver(T: ProgramTypes.Transition)(L: ProgramTypes.Location with type t = T.location and type comparator_witness = T.location_comparator_witness): sig
  (** A set of transitions. *)

  type elt = T.t
  type elt_comparator_witness = T.comparator_witness

  include SetCreators'0
    with type elt := elt
     and type elt_comparator_witness := elt_comparator_witness

  include ProgramTypes.TransitionSet
    with type elt := elt
     and type elt_comparator_witness := elt_comparator_witness
     and type location = L.t
     and type location_comparator_witness = L.comparator_witness
end

include ProgramTypes.Transition
  with type location = Location.t
   and type location_comparator_witness = Location.comparator_witness
   and type transition_label = TransitionLabel_.t
   and type transition_label_comparator_witness = TransitionLabel_.comparator_witness

(** Can be used to dump the transition into a koat file *)
val to_file_string: t -> string
