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
  include module type of Transition_.TransitionOver(ProbabilisticTransitionLabel)(Location)

  (** Returns true if both transitions belong to the same general transition, i.e. they have the same gt_id *)
  val same_gt: t -> t -> bool
end

module ProbabilisticTransitionNonProbOverappr: sig
  include module type of Transition_.TransitionOver(ProbabilisticTransitionLabelNonProbOverappr)(Location)
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
  val targets: t -> Set.Make(Location).t
  val guard: t -> Guard.t
  val invariant: t -> Guard.t
  val guard_without_inv: t -> Guard.t
  val transitions: t -> Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
  val same: t -> t -> bool
  val gt_id: t -> int
  val to_id_string: t -> string
  val cost: t -> Polynomials.Polynomial.t

  val add_invariant: t -> Guard.t -> t

  val to_string: t -> string
  val to_string_pretty: t -> string

  (** defaults to compare_same *)
  val compare: t -> t -> int

  (** Compare using general transition id *)
  val compare_same: t -> t -> int

  val vars: t -> VarSet.t
  val input_vars: t -> VarSet.t

  (** all locations, i.e. the input location and all target locations *)
  val locations: t -> Set.Make(Location).t

  val remove_non_contributors: VarSet.t -> t -> t

  (** map over all contained probabilistic transitions *)
  val map_transitions: (ProbabilisticTransition.t -> ProbabilisticTransition.t) -> t -> t
end

module GeneralTransitionSet: sig
  include ProgramTypes.TransitionSet
    with type elt = GeneralTransition.t
     and type t = Set.Make(GeneralTransition).t
     and type location_set = Set.Make(Location).t

  val all_transitions: t -> Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
end

module ProbabilisticTransitionGraph:
  ProgramTypes.TransitionGraph
    with type location = Location.t
     and type location_set = Set.Make(Location).t
     and type transition_set = Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
     and type transition_label = ProbabilisticTransitionLabel.t

module ProbabilisticProgram: sig
  include ProgramTypes.Program
    with type location = Location.t
     and type transition_label = ProbabilisticTransitionLabel.t
     and type location_set = Set.Make(Location).t
     and type transition_set = Transition_.TransitionSetOver(ProbabilisticTransition)(Location).t
     and type transition_graph = ProbabilisticTransitionGraph.t

  val from_gts: Location.t -> GeneralTransitionSet.t -> t

  (** The general transitions of the program *)
  val gts: t -> GeneralTransitionSet.t

  val pre_gt_cached: t -> GeneralTransition.t -> GeneralTransitionSet.t

  (* Restores legacy semantics for updates with distributions, i.e. updates of the form X->UNIFORM(0,1) are interpreted as X->X+UNIFORM(0,1) *)
  val restore_legacy_distribution_update_semantics: t -> t

  val to_string_pretty: t -> string
end

module ProbabilisticTransitionGraphNonProbOverappr: ProgramTypes.TransitionGraph
  with type location = Location.t
   and type location_set = Set.Make(Location).t
   and type transition_set = Transition_.TransitionSetOver(ProbabilisticTransitionNonProbOverappr)(Location).t
   and type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t

module ProbabilisticProgramNonProbOverappr: sig
  include ProgramTypes.Program
    with type location = Location.t
     and type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t
     and type location_set = Set.Make(Location).t
     and type transition_set =
           Transition_.TransitionSetOver(ProbabilisticTransitionNonProbOverappr)(Location).t
     and type transition_graph = ProbabilisticTransitionGraphNonProbOverappr.t
     and type t = ProbabilisticProgram.t
end
