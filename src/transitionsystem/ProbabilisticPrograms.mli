open Batteries

module ProbabilisticTransitionLabel: sig
  include ProgramTypes.TransitionLabel
    with type update_element = UpdateElement_.t

  val probability: t -> OurFloat.t
end

module ProbabilisticTransitionLabelNonProbOverappr: sig
  include ProgramTypes.TransitionLabel
    with type update_element = Polynomials.Polynomial.t
end

module ProbabilisticTransition: sig
  include ProgramTypes.Transition
    with type location = Location.t
     and type transition_label = ProbabilisticTransitionLabel.t

  (** Returns true if both transitions belong to the same general transition, i.e. they have the same gt_id *)
  val same_gt: t -> t -> bool
end

module ProbabilisticTransitionNonProbOverappr: sig
  include ProgramTypes.Transition
    with type location = Location.t
     and type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t
end

module GeneralTransition: sig
  type t

  val mk: start:Location.t ->
          fill_up_to_num_arg_vars: int ->
          patterns:Var.t list ->
          cost:Polynomials.Polynomial.t ->
          guard:Guard.t ->
          rhss: (OurFloat.t * UpdateElement_.t list * Location.t) list ->
          t

  val src: t -> Location.t
  val targets: t -> Location.LocationSetOver(Location).t
  val guard: t -> Guard.t
  val invariant: t -> Guard.t
  val guard_without_inv: t -> Guard.t
  val transitions: t -> Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
  val gt_id: t -> int
  val to_id_string: t -> string
  val to_id_string_pretty: t -> string
  val cost: t -> Polynomials.Polynomial.t

  val add_invariant: t -> Guard.t -> t

  val to_string: t -> string
  val to_string_pretty: t -> string

  (* Obtain a string only containing the transitoin's id, e.g., [ "g3" ] *)
  val ids_to_string: ?pretty:bool -> t -> string

  (** compare gt_ids*)
  val equal: t -> t -> bool
  val compare: t -> t -> int

  val hash: t -> int

  val sexp_of_t: t -> Sexplib0.Sexp.t

  val vars: t -> VarSet.t
  val input_vars: t -> VarSet.t

  (** all locations, i.e. the input location and all target locations *)
  val locations: t -> Location.LocationSetOver(Location).t

  val remove_non_contributors: VarSet.t -> t -> t

  (** map over all contained probabilistic transitions *)
  val map_transitions: (ProbabilisticTransition.t -> ProbabilisticTransition.t) -> t -> t

  include Base.Comparator.S with type t := t
end

module GeneralTransitionSet: sig
  include ProgramTypes.TransitionSet
    with type elt = GeneralTransition.t
     and type comparator_witness = GeneralTransition.comparator_witness
     and type location_set = (Location.t, Location.comparator_witness) Base.Set.t

  include module type of OurBase.MakeSetCreators0(GeneralTransition)

  (** Returns a string representing the transition set. *)
  val to_string : t -> string

  (** Returns a short string representing the transition set. *)
  val to_id_string : t -> string

  type location_set = (Location.t, Location.comparator_witness) Base.Set.t
  val locations: t -> location_set

  val all_transitions: t -> Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
end

module ProbabilisticTransitionGraph:
  ProgramTypes.TransitionGraph
    with type location = Location.t
     and type location_set = Location.LocationSetOver(Location).t
     and type transition_set = Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
     and type transition_label = ProbabilisticTransitionLabel.t

module ProbabilisticProgram: sig
  include ProgramTypes.Program
    with type location = Location.t
     and type transition_label = ProbabilisticTransitionLabel.t
     and type location_set = Location.LocationSetOver(Location).t
     and type transition_set = Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
     and type transition_graph = ProbabilisticTransitionGraph.t

  val from_gts: Location.t -> GeneralTransitionSet.t -> t

  (** The general transitions of the program *)
  val gts: t -> GeneralTransitionSet.t

  (** Enum of general transitions that form SCCs *)
  (** At least one transition of the general transition needs to be contained in the SCC so that *)
  (** the general transition itself is also viewed as part of the SCC. *)
  (** The resulting enum is in topological order *)
  val sccs_gts : t -> GeneralTransitionSet.t List.t

  val pre_gt_cached: t -> GeneralTransition.t -> GeneralTransitionSet.t

  (* Restores legacy semantics for updates with distributions, i.e. updates of the form X->UNIFORM(0,1) are interpreted as X->X+UNIFORM(0,1) *)
  val restore_legacy_distribution_update_semantics: t -> t

  val to_string_pretty: t -> string

  (** is the given general transition initial, i.e., does it start in the initial location *)
  val is_initial_gt: t -> GeneralTransition.t -> bool
end

module ProbabilisticTransitionGraphNonProbOverappr: ProgramTypes.TransitionGraph
  with type location = Location.t
   and type location_set = Location.LocationSetOver(Location).t
   and type transition_set = Transition_.TransitionSetOver(ProbabilisticTransitionNonProbOverappr)(Location).t
   and type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t

module ProbabilisticProgramNonProbOverappr: sig
  include ProgramTypes.Program
    with type location = Location.t
     and type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t
     and type location_set = Location.LocationSetOver(Location).t
     and type transition_set =
           Transition_.TransitionSetOver(ProbabilisticTransitionNonProbOverappr)(Location).t
     and type transition_graph = ProbabilisticTransitionGraphNonProbOverappr.t
     and type t = ProbabilisticProgram.t
end

(** RV Types for transitions *)

(** General Transitions *)
module GRV: ProgramTypes.RV with type RVTuple_.transition = GeneralTransition.t * Location.t

module RVTuple_:
  ProgramTypes.RVTuple with type transition = ProbabilisticTransition.t
module RVTupleNonProbOverappr_:
  ProgramTypes.RVTuple with type transition = ProbabilisticTransitionNonProbOverappr.t


module ProbabilisticRV: ProgramTypes.RV with type RVTuple_.transition = ProbabilisticTransition.t

module ProbabilisticRVNonProbOverappr:
  ProgramTypes.RV with type RVTuple_.transition = ProbabilisticTransitionNonProbOverappr.t

module Equalities: sig
  (** Some Equalities *)
  val t_eq: (ProbabilisticTransitionNonProbOverappr.t,ProbabilisticTransition.t) Util.TypeEq.t
  module RVTupleTypeCoercion: sig
    module A = ProbabilisticRVNonProbOverappr.RVTuple_
    module B = ProbabilisticRV.RVTuple_

    module Coerce(F: functor(_:ProgramTypes.RVTuple) -> sig type t end): sig
      val proof: (F(A).t,F(B).t) Util.TypeEq.t
    end
  end
end
