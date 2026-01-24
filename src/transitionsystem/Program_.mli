open! OurBase
(** Provides default module to handle programs. *)

exception RecursionNotSupported
(** KoAT does not support recursion yet *)

module Make
    (TL : ProgramTypes.DefaultTransitionLabel)
    (T :
      ProgramTypes.Transition
        with type transition_label = TL.t
         and type transition_label_comparator_witness = TL.comparator_witness)
    (G :
      ProgramTypes.TransitionGraph
        with type transition_label = TL.t
         and type transition_label_comparator_witness = TL.comparator_witness)
    (_ :
      ProgramTypes.PreAdapter
        with type transition_label = TL.t
         and type transition_label_comparator_witness = TL.comparator_witness
         and type transition = Location.t * TL.t * Location.t
         and type transition_comparator_witness = T.comparator_witness
         and type transition_graph = G.t)
    (_ : DependencyGraphAdapter.Adapter with type transition_label = TL.t and type transition_graph = G.t) : sig
  include
    ProgramTypes.Program
      with type transition_label = TL.t
       and type transition_label_comparator_witness = TL.comparator_witness
       and type transition_graph = G.t

  val from_sequence :
    Location.t -> ?return_locations:LocationSet.t -> ?rec_locations:LocationSet.t -> T.t Sequence.t -> t

  val remove_transition : t -> transition -> t
  val map_graph : (transition_graph -> transition_graph) -> t -> t

  (** {1 These are just dummy values to shadow the definitions from ProgramTypes.Program } *)

  val add_invariant : unit
  val simplify_all_guards : unit
  val remove_unsatisfiable_transitions : unit
  val remove_non_contributors : unit
end

module ClassicalProgram : sig
  include
    ProgramTypes.ClassicProgram
      with type transition_label = TransitionLabel_.t
       and type transition_label_comparator_witness = TransitionLabel_.comparator_witness
       and type transition_graph = TransitionGraph_.t

  val map_graph : (transition_graph -> transition_graph) -> t -> t

  val from_sequence :
    Location.t ->
    ?return_locations:LocationSet.t ->
    ?rec_locations:LocationSet.t ->
    transition Sequence.t ->
    t

  val from_graph :
    Location.t -> ?return_locations:LocationSet.t -> ?rec_locations:LocationSet.t -> transition_graph -> t

  val remove_transition : t -> transition -> t
  val remove_non_contributors : VarSet.t -> t -> t
end

include module type of ClassicalProgram

val from_com_transitions :
  ?termination:bool -> ?return_locations:LocationSet.t -> Transition_.t list list -> Location.t -> t
(** Creates a program from a list of transitions and a (start) location.
     A list of k transitions makes up a Com_k transition
     Since KoAT currently does not support recursion we try to eliminate it.
     If this is not possible we throw a RecursionNotSupportedException *)

val rename : t -> t
(** TODO doc *)

val to_file_string : t -> string

val to_file : ?file:string option -> t -> unit
(** Creates a file (if it does not already exist) and writes the program into it. *)
