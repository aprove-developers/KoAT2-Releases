open! OurBase

(** A transition connects two locations and is labeled with a transition label.*)

(** Creates a transition over a given label and location type *)
module Make (TL : ProgramTypes.TransitionLabel) : sig
  include
    ProgramTypes.Transition
      with type transition_label = TL.t
       and type transition_label_comparator_witness = TL.comparator_witness
end

(** Creates a classical transition over a given transition label and location type *)
module MakeClassical (TL : ProgramTypes.ClassicalTransitionLabel) : sig
  include
    ProgramTypes.ClassicalTransition
      with type transition_label = TL.t
       and type transition_label_comparator_witness = TL.comparator_witness
end

module TransitionSetOver (T : ProgramTypes.Transition) : sig
  (** A set of transitions. *)

  type elt = T.t
  type elt_comparator_witness = T.comparator_witness

  include SetCreators'0 with type elt := elt and type elt_comparator_witness := elt_comparator_witness

  include
    ProgramTypes.TransitionSet with type elt := elt and type elt_comparator_witness := elt_comparator_witness
end

include
  ProgramTypes.ClassicalTransition
    with type transition_label = TransitionLabel_.t
     and type transition_label_comparator_witness = TransitionLabel_.comparator_witness

val to_file_string : t -> string
(** Can be used to dump the transition into a koat file *)

val rename : RenameMap.t -> t -> t
