open OurBase

module UnliftedTimeBound : sig
  type ('trans, 'bound, 'trans_cmp_wit) unlifted_time_bound
  (** The abstract type *)

  module Make (PM : ProgramTypes.ProgramModules) (B : BoundType.Bound) : sig
    open PM

    type t = (Transition.t, B.t, Transition.comparator_witness) unlifted_time_bound

    type compute_proof =
      get_timebound:(Transition.t -> B.t) ->
      get_sizebound:(Transition.t -> Var.t -> B.t) ->
      (Transition.t, B.t, Transition.comparator_witness) Map.t ->
      B.t ->
      Formatter.format ->
      FormattedString.t
    (** Useful for computation of proofs when lifting bounds.
        The map of the last argument maps all entry transitions to an unlifted (local) time bound for the [measure_decr_transitions]. *)

    val measure_decr_transitions : t -> (Transition.t, Transition.comparator_witness) Set.t
    (** All transitions for which we can compute an unlifted (local) time bound *)

    val entry_transitions_measure : t -> (Transition.t, B.t, Transition.comparator_witness) Map.t

    val mk :
      measure_decr_transitions:(Transition.t, Transition.comparator_witness) Set.t ->
      ?compute_proof:compute_proof Option.t ->
      (Transition.t, B.t, Transition.comparator_witness) Map.t ->
      t
    (** Create a new unlifted time bound. The transitions in [measure_decr_transitions] are those for which a new time bound can be derived. *)

    val mk_from_program :
      Logger.log ->
      handled_transitions:(Transition.t, 'a) Base.Set.t ->
      measure_decr_transitions:(Transition.t, Transition.comparator_witness) Base.Set.t ->
      ?compute_proof:compute_proof Option.t ->
      Program.t ->
      (Transition.t -> B.t) ->
      t
    (** Similar to before but explicitly computes the map mapping entry transitions to unlifted (local) time bounds.
          Here, the entry transitions are considered to be all transitions that are pre transitions of [handled_transitions] but not contained in [handled_transitions] itself.
          All computed entry transitions are then logged via the provided logger. *)

    val lift : get_sizebound:(Transition.t -> Var.t -> B.t) -> get_timebound:(Transition.t -> B.t) -> t -> B.t
    (** Lifs the unlifted time bound to global time bound.
          To that end, we overapproximate the measure assigned to all entry locations using corresponding sizebounds and then multiply this with incoming time bounds before summing up all bounds obtained in this manner. *)

    val lift_and_get_proof :
      get_timebound:(Transition.t -> B.t) ->
      get_sizebound:(Transition.t -> Var.t -> B.t) ->
      t ->
      B.t * (Formatter.format -> FormattedString.t)
    (** Analoguous to [lift] but provides an additional returned value which uses the registered [compute_proof] function in the context of the computation of [lift] to generate a proof.
          To that end this function is supplied [get_timebound], [get_sizebound] the map of entry transitions to unlifted (local) time bounds and the global time bound obtained by lifting. *)
  end
end
