open! OurBase

type label_without_backlink = {
  probability : Polynomials.RationalLaurentPolynomial.t;
  overappr_guard : Guard.t;
  update : UpdateElement_.t ProgramTypes.VarMap.t;
  overappr_nonprob_update : Polynomials.Polynomial.t ProgramTypes.VarMap.t;
}

module ProbabilisticTransitionLabel : sig
  include ProgramTypes.TransitionLabel with type update_element = UpdateElement_.t

  val probability : t -> Polynomials.RationalLaurentPolynomial.t
  val without_backlink : t -> label_without_backlink
end

module ProbabilisticTransitionLabelNonProbOverappr : sig
  include ProgramTypes.ClassicalTransitionLabel with type update_element = Polynomials.Polynomial.t
end

type general_transition

(** Note that General Transitions are cyclic with Probabilistictransitions! So do not modify Probabilistictransitions directly! *)
module ProbabilisticTransition : sig
  include
    ProgramTypes.Transition
      with type transition_label = ProbabilisticTransitionLabel.t
       and type transition_label_comparator_witness = ProbabilisticTransitionLabel.comparator_witness

  val same_gt : t -> t -> bool
  (** Returns true if both transitions belong to the same general transition, i.e. they have the same gt_id *)

  val without_backlink : t -> Location.t * label_without_backlink * Location.t

  val gt : t -> general_transition
  (** Obtain the general transition from the program that contains this transition *)
end

module ProbabilisticTransitionNonProbOverappr : sig
  include
    ProgramTypes.ClassicalTransition
      with type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t
       and type transition_label_comparator_witness =
        ProbabilisticTransitionLabelNonProbOverappr.comparator_witness
end

(** Note that General Transitions are cyclic with Probabilistictransitions! So do not modify Probabilistictransitions directly! *)
module GeneralTransition : sig
  type t = general_transition

  val mk :
    start:Location.t ->
    fill_up_to_num_arg_vars:int ->
    patterns:Var.t list ->
    cost:Polynomials.Polynomial.t ->
    guard:Guard.t ->
    rhss:(Polynomials.RationalLaurentPolynomial.t * UpdateElement_.t list * Location.t) list ->
    t

  val mk_from_labels_without_backlink :
    start:Location.t ->
    guard_without_invariant:Guard.t ->
    invariant:Guard.t ->
    cost:Polynomials.Polynomial.t ->
    rhss:(label_without_backlink * Location.t) List.t ->
    t
  (** Similar to [mk] but higher-level.
      Note that we assume the provided updates to be {i complete}, i.e., we do not fill up the argument variables *)

  val src : t -> Location.t
  val targets : t -> LocationSet.t
  val guard : t -> Guard.t
  val invariant : t -> Guard.t
  val guard_without_inv : t -> Guard.t
  val transitions : t -> (ProbabilisticTransition.t, ProbabilisticTransition.comparator_witness) Set.t

  val transitions_to_target :
    Location.t -> t -> (ProbabilisticTransition.t, ProbabilisticTransition.comparator_witness) Set.t

  val is_probabilistic : t -> bool
  (** tests if the general transition contains probabilistic behaviour (i.e., probabilistic updates or probabilistic branching) *)

  val gt_id : t -> int
  val to_id_string : t -> string
  val to_id_string_pretty : t -> string
  val cost : t -> Polynomials.Polynomial.t
  val add_invariant : t -> Guard.t -> t
  val to_string : t -> string
  val to_string_pretty : t -> string
  val to_file_string : t -> string

  val ids_to_string : ?pretty:bool -> t -> string
  (** Obtain a string only containing the transition's id, e.g., [ "g3" ] *)

  val equal : t -> t -> bool
  (** compare gt_ids*)

  val compare : t -> t -> int
  val hash : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val vars : t -> VarSet.t
  val input_vars : t -> VarSet.t

  val locations : t -> LocationSet.t
  (** all locations, i.e. the input location and all target locations *)

  val map_transitions : (ProbabilisticTransition.t -> ProbabilisticTransition.t) -> t -> t
  (** map over all contained probabilistic transitions *)

  include Comparator.S with type t := t
end

module GeneralTransitionSet : sig
  include
    ProgramTypes.TransitionSet
      with type elt = GeneralTransition.t
       and type elt_comparator_witness = GeneralTransition.comparator_witness

  include module type of MakeSetCreators0 (GeneralTransition)

  val to_string : t -> string
  (** Returns a string representing the transition set. *)

  val to_id_string : t -> string
  (** Returns a short string representing the transition set. *)

  val locations : t -> LocationSet.t
  val all_transitions : t -> Transition_.TransitionSetOver(ProbabilisticTransition).t
  val of_tset : (ProbabilisticTransition.t, 'a) Set.t -> t
end

module ProbabilisticTransitionGraph : sig
  include
    ProgramTypes.TransitionGraph
      with type transition_label = ProbabilisticTransitionLabel.t
       and type transition_label_comparator_witness = ProbabilisticTransitionLabel.comparator_witness

  val outgoing_gts : t -> Location.t -> GeneralTransitionSet.t
  val gts : t -> GeneralTransitionSet.t
end

module ProbabilisticProgram : sig
  include
    ProgramTypes.Program
      with type transition_label = ProbabilisticTransitionLabel.t
       and type transition_label_comparator_witness = ProbabilisticTransitionLabel.comparator_witness
       and type transition_graph = ProbabilisticTransitionGraph.t

  val from_gts : Location.t -> GeneralTransitionSet.t -> t
  val succ_gts : t -> Location.t -> GeneralTransitionSet.t

  val gts : t -> GeneralTransitionSet.t
  (** The general transitions of the program *)

  val sccs_gts : t -> GeneralTransitionSet.t List.t
  (** Enum of general transitions that form SCCs
      At least one transition of the general transition needs to be contained in the SCC so that
      the general transition itself is also viewed as part of the SCC.
      The resulting enum is in topological order *)

  val scc_gts_from_locs : t -> LocationSet.t -> GeneralTransitionSet.t
  (** The general transitions that make up the SCC given by the [LocationSet] *)

  val scc_gts_from_locs_with_incoming_and_outgoing : t -> LocationSet.t -> GeneralTransitionSet.t
  (** Similar to [scc_gts_from_locs] but includes incoming and outgoing general transitions of the SCC *)

  val pre_gt : t -> GeneralTransition.t -> GeneralTransitionSet.t
  val remove_zero_prob_transitions : (transition, transition_comparator_witness) Set.t -> t -> t

  (* Restores legacy semantics for updates with distributions, i.e. updates of the form X->UNIFORM(0,1) are interpreted as X->X+UNIFORM(0,1) *)
  val restore_legacy_distribution_update_semantics : t -> t
  val to_string_pretty : t -> string

  val is_initial_gt : t -> GeneralTransition.t -> bool
  (** is the given general transition initial, i.e., does it start in the initial location *)
end

module ProbabilisticTransitionGraphNonProbOverappr :
  ProgramTypes.TransitionGraph
    with type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t
     and type transition_label_comparator_witness =
      ProbabilisticTransitionLabelNonProbOverappr.comparator_witness

module ProbabilisticProgramNonProbOverappr : sig
  include
    ProgramTypes.Program
      with type transition_label = ProbabilisticTransitionLabelNonProbOverappr.t
       and type transition_label_comparator_witness =
        ProbabilisticTransitionLabelNonProbOverappr.comparator_witness
       and type transition_graph = ProbabilisticTransitionGraphNonProbOverappr.t
end

(** {1 RV Types for transitions} *)

module ProbabilisticRV :
  ProgramTypes.RV
    with type transition = ProbabilisticTransition.t
     and type transition_comparator_witness = ProbabilisticTransition.comparator_witness

module ProbabilisticRVNonProbOverappr :
  ProgramTypes.RV
    with type transition = ProbabilisticTransitionNonProbOverappr.t
     and type transition_comparator_witness = ProbabilisticTransitionNonProbOverappr.comparator_witness

module GRV : sig
  include ProgramTypes.RV with type transition = GeneralTransition.t * Location.t

  val gt : t -> GeneralTransition.t
  val to_probabilistic_rvs : t -> ProbabilisticRV.t Sequence.t
end

module Equalities : sig
  (** Some Equalities *)

  val trans_eq : (ProbabilisticTransitionNonProbOverappr.t, ProbabilisticTransition.t) Type_equal.t
  val rvtuple__eq : (ProbabilisticRVNonProbOverappr.t, ProbabilisticRV.t) Type_equal.t

  val trans_cmp_wit_eq :
    ( ProbabilisticTransitionNonProbOverappr.comparator_witness,
      ProbabilisticTransition.comparator_witness )
    Type_equal.t

  val rvtuple__cmp_wit_eq :
    (ProbabilisticRVNonProbOverappr.comparator_witness, ProbabilisticRV.comparator_witness) Type_equal.t

  val program_equalities : (ProbabilisticProgram.t, ProbabilisticProgramNonProbOverappr.t) Type_equal.t
end
