open Batteries

(** A transition connects two locations and is labeled with an updated function and a guard. *)

(** Creates a transition over a given location type *)
module TransitionOver(TL: ProgramTypes.TransitionLabel)(L: ProgramTypes.Location) : sig
  include ProgramTypes.Transition
    with type location = L.t
     and type transition_label = TL.t
end

module TransitionSetOver(T: ProgramTypes.Transition)(L: ProgramTypes.Location with type t = T.location): sig
  (** A set of transitions. *)
  (* include module type of Base.Set with type ('a,'b) t :=  ('a,'b) base_set_t *)

  include OurBase.Creators'0
    with type elt = T.t
     and type elt_comparator_witness = T.comparator_witness

  include ProgramTypes.TransitionSet
    with type elt = T.t
     and type elt_comparator_witness = T.comparator_witness
     and type location_set = (L.t, L.comparator_witness) Base.Set.t

end


include ProgramTypes.Transition
  with type location = Location.t
   and type transition_label = TransitionLabel_.t
   and type comparator_witness = TransitionOver(TransitionLabel_)(Location).comparator_witness

(** Can be used to dump the transition into a koat file *)
val to_file_string: t -> string
