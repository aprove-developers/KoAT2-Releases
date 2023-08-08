open OurBase

module UnliftedTimeBound: sig
    (** The abstract type *)
    type ('trans,'bound,'trans_cmp_wit) unlifted_time_bound

    module Make(PM: ProgramTypes.ProgramModules)(B: BoundType.Bound): sig
      open PM

      type t = (Transition.t, B.t, Transition.comparator_witness) unlifted_time_bound

      (** All transitions for which we can compute an unlifted (local) time bound *)
      val measure_decr_transitions: t -> (Transition.t,Transition.comparator_witness) Set.t

      (** Create a new unlifted time bound. The transitions in [measure_decr_transitions] are those for which a new time bound can be derived.
          The [hook] is only stored but not executed by this module.
          Useful for adding proofs.
          The map of the last argument maps all entry transitions to an unlifted (local) time bound for the [measure_decr_transitions]. *)
      val mk: measure_decr_transitions:((Transition.t,Transition.comparator_witness) Set.t)
              -> ?hook:(get_timebound:(Transition.t -> B.t)
                        -> get_sizebound:(Transition.t -> Var.t -> B.t)
                        -> (Transition.t, B.t, Transition.comparator_witness) Base.Map.t
                        -> B.t -> unit) option
              -> (Transition.t,B.t,Transition.comparator_witness) Map.t
              -> t

      (** Similar to before but explicitly computes the map mapping entry transitions to unlifted (local) time bounds.
          Here, the entry transitions are considered to be all transitions that are pre transitions of [handled_transitions] but not contained in [handled_transitions] itself.
          All computed entry transitions are then logged via the provided logger. *)
      val mk_from_program: Logger.log
                           -> handled_transitions:(Transition.t, 'a) Base.Set.t
                           -> measure_decr_transitions:(Transition.t, Transition.comparator_witness) Base.Set.t
                           -> ?hook:(get_timebound:(Transition.t -> B.t) ->
                                    get_sizebound:(Transition.t -> Var.t -> B.t) ->
                                    (Transition.t, B.t, Transition.comparator_witness) Base.Map.t ->
                                    B.t -> unit)
                                   option
                           -> Program.t -> (Transition.t -> B.t) -> t

      (** Lifs the unlifted time bound to global time bound.
          To that end, we overapproximate the measure assigned to all entry locations using corresponding sizebounds and then multiply this with incoming time bounds before summing up all bounds obtained in this manner. *)
      val lift: get_sizebound:(Transition.t -> Var.t -> B.t)
                -> get_timebound:(Transition.t -> B.t) -> t -> B.t

      (** Analoguously to [lift] but provides an additional returned value which executes the hook in the context of the computation of [lift].
          To that end the hook function is supplied [get_timebound], [get_sizebound] the map of entry transitions to unlifte (local) time bounds and the global time bound obtained by lifting. *)
      val lift_and_get_hook: get_timebound:(Transition.t -> B.t)
                             -> get_sizebound:(Transition.t -> Var.t -> B.t)
                             -> t -> B.t * (unit -> unit)
    end
end
