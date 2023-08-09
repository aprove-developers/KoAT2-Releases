open OurBase
(** Provides default module to handle programs. *)

open Polynomials

open Constraints
(** Provides default module to handle programs. *)

exception RecursionNotSupported
(** KoAT does not support recursion yet *)

module Make
    (TL : ProgramTypes.TransitionLabel)
    (T : ProgramTypes.Transition
           with type transition_label = TL.t
            and type transition_label_comparator_witness = TL.comparator_witness)
    (L : ProgramTypes.Location
           with type t = T.location
            and type comparator_witness = T.location_comparator_witness)
    (G : ProgramTypes.TransitionGraph
           with type location = L.t
            and type location_comparator_witness = L.comparator_witness
            and type transition_label = TL.t
            and type transition_label_comparator_witness = TL.comparator_witness) : sig
  include
    ProgramTypes.Program
      with type location = L.t
       and type location_comparator_witness = L.comparator_witness
       and type transition_label = TL.t
       and type transition_label_comparator_witness = TL.comparator_witness
       and type transition_graph = G.t
end

module ProgramOverLocation (L : ProgramTypes.Location) : sig
  include
    ProgramTypes.Program
      with type location = L.t
       and type location_comparator_witness = L.comparator_witness
       and type transition_label = TransitionLabel_.t
       and type transition_label_comparator_witness = TransitionLabel_.comparator_witness
       and type transition_graph = TransitionGraph_.TransitionGraphOverLocation(L).t
end

include module type of ProgramOverLocation (Location)

val from_com_transitions : ?termination:bool -> Transition_.t list list -> Location.t -> t
(** Creates a program from a list of transitions and a (start) location.
     A list of k transitions makes up a Com_k transition
     Since KoAT currently does not support recursion we try to eliminate it.
     If this is not possible we throw a RecursionNotSupportedException *)

val rename : t -> t
(** TODO doc *)

val to_file : t -> string -> unit
(** Creates a file (if it does not already exist) and writes the program into it. *)
