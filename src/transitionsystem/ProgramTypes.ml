open OurBase
open Constraints
open Polynomials
(** Provides commonly used module types in programs *)

type 'a var_map = (Var.t, 'a, Var.comparator_witness) Map.t

(** A location is a node of a transition system and can be connected to other locations via transitions. *)
module type Location = sig
  (** Type of location, we use strings. *)
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  (** Generates a hash value for a location.*)
  val hash : t -> int

  (** Returns a string representing a location. *)
  val to_string : t -> string

  include Comparator.S with type t := t
  val sexp_of_t: t -> Sexp.t
end

module type LocationSet = sig
  include SetCreators'0

  (** Returns a string representing the transition set. *)
  val to_string : t -> string
end

module type TransitionSet = sig
  (** A set of transitions. *)

  include SetCreators'0

  (** Returns a string representing the transition set. *)
  val to_string : t -> string

  (** Returns a short string representing the transition set. *)
  val to_id_string : t -> string

  type location
  type location_comparator_witness
  type location_set = (location,location_comparator_witness) Set.t

  val locations: t -> location_set

  (** Returns a locationSet corresponding to the targets of all transitions contained in the set passed as first argument *)
  val targets: t -> location_set

  val find_by_id: t -> int -> elt Option.t

  val find_by_ids: t -> int Sequence.t -> t
end

module type TransitionLabel = sig
  (** A transition label consists of an unique id, an update function, a guard and a cost function. *)
  type t
  type update_element
  module Invariant = Guard

  (** Returns a default label with id 0, [true] as the guard,no update function and the default cost function. *)
  val default: t

  (** Returns the update map of the transitionlabel *)
  val update_map : t -> update_element var_map

  (** Returns the update of a variable. *)
  val update : t -> Var.t -> update_element Option.t

  (** Returns if the two labels describe the same transition *)
  val equivalent: t -> t -> bool

  (* are both labels ids identical? *)
  val equal: t -> t -> bool

  (** TODO doc *)
  val compare_equivalent: t -> t -> int

  (** Compare labels by ids *)
  val compare: t -> t -> int

  val equivalent_update: t -> t -> bool

  (** Returns the guard of the label. *)
  val guard: t -> Guard.t

  (** Returns the guard of the label without considering invariants. *)
  val guard_without_inv : t -> Guard.t

  (** Returns the invariant. *)
  val invariant : t -> Invariant.t

  (** Apply function to guard *)
  val map_guard: (Guard.t -> Guard.t) -> t -> t

  (** Returns the unique id. *)
  val id: t -> int

  (** Returns a string with the transitions id (or ids in case of probabilistic programs). *)
  (** Used for printing graphs. *)
  val ids_to_string: ?pretty:bool -> t -> string

  val add_invariant: t -> Invariant.t -> t

  (** Returns the cost function *)
  val cost : t -> Polynomials.Polynomial.t

  val negative_costs : t -> bool

  (** Returns a string representing the label. *)
  val to_string : ?pretty:bool -> t -> string

  (** Returns a string representing the left hand side of the update function. *)
  val update_to_string_lhs : t -> string

  (** Returns a string representing the right hand side of the update function. *)
  val update_to_string_rhs : t -> string

  val update_to_string_lhs_pretty : t -> string

  val update_to_string_rhs_pretty : t -> string

  (** Returns a string representing the cost. *)
  val cost_to_string : t -> string

  (** Returns a string representing the id of the label. *)
  val to_id_string : t -> string

  (** The call {i fill_up_arg_vars_up_to_num n} adds trivial updates for the first {i n}, i.e. Arg_0, .., Arg_n-1 arguments that are not contained in the labels update map *)
  val fill_up_arg_vars_up_to_num: int -> t -> t

  (** TODO doc *)
  val rename : RenameMap.t -> t -> t

  (** Rename temporary variables to identifiers provided by the (possibly infinite) sequence *)
  val rename_temp_vars : t -> Var.t Sequence.t -> t

  (** Returns the set of variables. *)
  val vars : t -> VarSet.t

  (** Returns the set of input variables of the transition, i.e. the non temporary variables  *)
  val input_vars : t -> VarSet.t

  (** Returns the set of temporay variables of the transition.  *)
  val tmp_vars : t -> VarSet.t

  (** Returns the number of input variables *)
  val input_size : t -> int

  val has_tmp_vars : t -> bool

  (** Keeps only the atoms of the guard whose variables are a subset of non_static *)
  val relax_guard : non_static:VarSet.t -> t -> t

  (** All input variables where the update is not x' = x.*)
  val changed_vars : t -> VarSet.t

  (** Guard that is true if both transitions can be executed one after another *)
  val chain_guards: t -> t -> Guard.t

  (** Overapproximates nonlinear updates by nondeterministic updates. Useful for Farkas lemma *)
  val overapprox_nonlinear_updates : t -> t

  val remove_non_contributors : VarSet.t -> t -> t

  include Comparator.S with type t := t
  val sexp_of_t : t -> Sexp.t
end

(** A transition connects two locations and is labeled with an updated function and a guard. *)
module type Transition = sig
  type location
  type location_comparator_witness

  type transition_label
  type transition_label_comparator_witness

  (** Type of a transition, i.e., two connected locations and a label. *)
  type t = location * transition_label * location

  val equal : t -> t -> bool

  (** compare transition ids *)
  val compare: t -> t -> int

  val equivalent : t -> t -> bool

  val compare_equivalent : t -> t -> int

  (** Generates a hash value for a transition. *)
  val hash : t -> int

  val overapprox_nonlinear_updates: t -> t

  (** Returns a string of the form id: src -> target. *)
  val to_id_string : t -> string
  val to_id_string_pretty : t -> string

  (** Returns a string representing the transition. *)
  val to_string : t -> string

  val to_string_pretty : t -> string

  (** Returns the source location of a transition. *)
  val src : t -> location

  (** Returns the label of a transition. *)
  val label : t -> transition_label

  (** Apply a function to a transitions label *)
  val map_label: (transition_label -> transition_label) -> t -> t

  (** Returns the target location of a transition. *)
  val target : t -> location

  (** Returns an (unique) id of a transition label. TODO doc unique??*)
  val id : t -> int

  (** Returns a cost function of a transition represented as a polynomial. *)
  val cost : t -> Polynomial.t

  (** Adds the invariant to this transition. *)
  val add_invariant : Constraint.t -> t -> t

  val rename : RenameMap.t -> t -> t

  include Comparator.S
    with type t := t
     and type comparator_witness =
           ( transition_label_comparator_witness
           , location_comparator_witness) TransitionComparator.comparator_witness
  val sexp_of_t: t -> Sexp.t
end

(** This module represents a transition graph. *)
module type TransitionGraph = sig
  type location
  type location_comparator_witness
  type location_set = (location,location_comparator_witness) Set.t

  type transition_label
  type transition_label_comparator_witness

  type transition = location * transition_label * location
  type transition_comparator_witness = (transition_label_comparator_witness,location_comparator_witness) TransitionComparator.comparator_witness

  type transition_set = (transition,transition_comparator_witness) Set.t


  include Graph.Sig.P with type V.t = location
    and type V.label = location
      and type E.t = transition
      and type E.label = transition_label

  (** Creates a transition graph from an enum of transitions. *)
  val mk : transition Sequence.t -> t

  (** Adds all locations from an enum to a transtion graph. *)
  val add_locations : location Sequence.t -> t -> t

  (** Adds all transitions from an enum to a transtion graph. Implicitly adds locations when they do not exit. *)
  val add_transitions : transition Sequence.t -> t -> t

  (** Apply function to the graphs transitions  *)
  val map_transitions: (transition -> transition) -> t -> t

  (** Apply function to the graphs labels  *)
  val map_labels: (transition_label -> transition_label) -> t -> t

  (** Returns the set of locations. *)
  val locations : t -> location_set

  (** Returns the set of transitions. *)
  val transitions : t -> transition_set

  (** Returns the set of transitions consisting of transitions which are in the program graph and where both source and target location are part of the given location list. *)
  val loc_transitions : t -> location list -> transition_set

  (** Checks fore equivalence TODO: lies everywhere...*)
  val equivalent : t -> t -> bool

  (** Replaces the first edge by the second edge. *)
  val replace_edge_e : transition -> transition -> t -> t

  (** Adds the invariant to the location of the graph. *)
  val add_invariant : location -> Constraint.t -> t -> t

  (** Returns the (biggest) strongly connected components of the transiton graph. *)
  val sccs : t -> transition_set list

  (** Returns the (biggest) strongly connected components of the transitons. *)
  val sccs_ : transition Sequence.t -> transition_set list
end

module type Program = sig
  type location
  type location_comparator_witness

  type transition_label
  type transition_label_comparator_witness

  type transition = location * transition_label * location
  type transition_comparator_witness = (transition_label_comparator_witness,location_comparator_witness) TransitionComparator.comparator_witness

  type location_set = (location,location_comparator_witness) Set.t
  type transition_set = (transition,transition_comparator_witness) Set.t

  type transition_graph

  (** Type of a program consisting of a program graph and a start location. *)
  type t =
    ( transition_label, transition_label_comparator_witness
    , location, location_comparator_witness
    , transition_graph) GenericProgram_.t

  (** Create a program from a start location and an enum of transitions. *)
  (** The user is responsible for making sure that the arities of all locations match and for correct naming of arg variables *)
  val from_sequence: location -> transition Sequence.t -> t

  (** Create a program from a start location and a graph *)
  val from_graph: location -> transition_graph -> t

  (** Removes the location from the program and all edges to it. *)
  val remove_location : t -> location -> t

  (** Removes a transition from a program. *)
  val remove_transition : t -> transition -> t

  (* Removes the transitions from a certain transitionset to a program *)
  val remove_transition_set: transition_set -> t -> t

  (** Apply function to the underlying TransitionGraph *)
  val map_graph : (transition_graph -> transition_graph) -> t -> t

  (** Apply function to the programs transitions  *)
  val map_transitions: (transition -> transition) -> t -> t

  (** Apply function to the programs labels  *)
  val map_labels: (transition_label -> transition_label) -> t -> t

  (** Returns transition graph of a program. *)
  val graph : t -> transition_graph

  (** Adds the invariant to a location of the program. *)
  val add_invariant : location -> Constraint.t -> t -> t

  (** Tries to simplify the guard of all transitions by invoking the SMT Solver *)
  val simplify_all_guards : t -> t

  (** Returns a set of all transitions which occur directly before the given transition in the graph.
      Corresponds to pre(t).
      Note that the computation involves calls to the SMT solver and is therefore expensive.
      However, to improve performance they are cached inside the program type [t]. *)
  val pre : t -> transition -> transition_set

  (** Similar to pre but return the pre-transitions as a lazy Sequence.
      If the result has already been computed and cached in the value of type [t], then we do not recompute the pre-transitions.
      If however the pre-transitions haven't been computed yet then they are lazily computed when inspecting the returned sequence. These values are *not* added to the cached!
      This is for instance useful when cutting unsatisfiable transitions since during preprocessing the cache is invalidated many times and it suffices to check whether one pre-transition exists. *)
  val pre_lazy: t -> transition -> transition Base.Sequence.t

  (** Returns true if the given transition is an initial transition. *)
  val is_initial : t -> transition -> bool

  (** Returns true if the given transition is an initial transition. *)
  val is_initial_location : t -> location -> bool

  (** Returns true if the program graphs are equivalent and both start locations are equal. *)
  val equivalent : t -> t -> bool

  (** Returns a formatted string representing the program. *)
  val to_formatted_string: ?pretty:bool -> t -> FormattedString.t

  (** Returns a string representing the program. *)
  val to_string : t -> string

  (** Input is not interpreted as a filepath, but as a program in simple mode. Method returns a string representation of a program from such an input. *)
  val to_simple_string : t -> string

  (** Returns all variables of the program. *)
  val vars : t -> VarSet.t

  (** Returns all input variables of the program. *)
  val input_vars : t -> VarSet.t

  (** Returns the set of temporay variables of the transition. *)
  val tmp_vars : t -> VarSet.t

  (** Returns all locations which occur in the transitions, but each location only once. *)
  val locations : t -> location_set

  (** Returns a set of all transitions which occur in the program graph of the program. *)
  val transitions : t -> transition_set

  (** Returns start location. *)
  val start : t -> location

  (** Returns the (biggest) strongly connected components of the transiton graph in topological order. *)
  val sccs : t -> transition_set List.t

  (** Returns all transitions which are parallel to a given transition. Thus, all transitions start in the same location and end in the same location. *)
  val parallel_transitions : t -> transition -> transition_set

  (** Returns all transitions, that belong to an SCC. *)
  val non_trivial_transitions : t -> transition_set

  (** Computes all entry transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  val entry_transitions : t -> transition list -> transition Batteries.List.t

  (** Like [entry_transitions] but loggs the results using the given logger *)
  val entry_transitions_with_logger: Batteries.Logger.log -> t -> transition list -> transition Batteries.List.t

  (** Computes all outgoing transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  val outgoing_transitions : Batteries.Logger.log -> t -> transition list -> transition Batteries.List.t

  (** Remove the given variables that do not contribute to the problem *)
  val remove_non_contributors: VarSet.t -> t -> t

  (** This module exposes internal structure. *)
  (** Useful for tests. *)
  module InternalTest: sig
    (** Obtain the internal cache of pre transitions *)
    val get_pre_cache: t -> (transition, transition_set) Hashtbl.t

    (** Compute pre transitions by ignoring the cache. *)
    val compute_pre: t -> transition -> transition Sequence.t
  end
end

module type RV = sig
  type transition
  type transition_comparator_witness

  type t = transition * Var.t

  include Comparator.S
    with type t := t
     and type comparator_witness = ( transition_comparator_witness
                                   , Var.comparator_witness) RVComparator.comparator_witness

  (** Returns the transition of the result variable. *)
  val transition : t -> transition

  (** Returns the variable of the result variable. *)
  val variable : t -> Var.t

  val to_id_string: t -> string
  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int
  val ids_to_string: ?pretty:bool -> t -> string

  val sexp_of_t : t -> Sexp.t
end

type !'a program_modules_meta

module type ProgramModules = sig
  module Location: Location
  module LocationSet: LocationSet
    with type elt = Location.t
     and type elt_comparator_witness = Location.comparator_witness

  module UpdateElement: PolyTypes.Polynomial
    with type value = OurInt.t

  module TransitionLabel: TransitionLabel
    with type update_element = UpdateElement.t

  module Transition: Transition
    with type location = Location.t
     and type location_comparator_witness = Location.comparator_witness
     and type transition_label = TransitionLabel.t
     and type transition_label_comparator_witness = TransitionLabel.comparator_witness

  module TransitionSet: TransitionSet
    with type elt = Transition.t
     and type elt_comparator_witness = (TransitionLabel.comparator_witness,Location.comparator_witness) TransitionComparator.comparator_witness
     and type location = Location.t
     and type location_comparator_witness = Location.comparator_witness

  module TransitionGraph: TransitionGraph
    with type location = Location.t
     and type location_comparator_witness = Location.comparator_witness
     and type transition_label = TransitionLabel.t
     and type transition_label_comparator_witness = TransitionLabel.comparator_witness
     and type transition = Transition.t

  module Program: Program
    with type location = Location.t
     and type location_comparator_witness = Location.comparator_witness
     and type transition_label = TransitionLabel.t
     and type transition_label_comparator_witness = TransitionLabel.comparator_witness
     and type transition = Transition.t
     and type transition_graph = TransitionGraph.t

  module RV: RV

  type program_modules_t = ( TransitionLabel.t
                           * TransitionLabel.comparator_witness
                           * Location.t
                           * Location.comparator_witness
                           * TransitionGraph.t)  program_modules_meta
end

(** For classical/non-probabilistic programs we want polynomial updates only and RV.transition = Transition.t *)
module type ClassicalProgramModules = sig
  include ProgramModules with module UpdateElement = Polynomial
  module RV: RV
    with type transition = Transition.t
     and type transition_comparator_witness = Transition.comparator_witness
end
