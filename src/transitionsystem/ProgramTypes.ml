open! OurBase
open Constraints

open Polynomials
(** Provides commonly used module types in programs *)

module VarMap = MakeMapCreators1 (Var)

module type TransitionSet = sig
  (** A set of transitions. *)

  include SetCreators'0

  val to_string : t -> string
  (** Returns a string representing the transition set. *)

  val to_id_string : t -> string
  (** Returns a short string representing the transition set. *)

  val to_id_string_pretty : t -> string
  (** [to_id_string] but prettier *)

  val locations : t -> LocationSet.t

  val targets : t -> LocationSet.t
  (** Returns a locationSet corresponding to the targets of all transitions contained in the set passed as first argument *)

  val find_by_id : t -> int -> elt Option.t
  val find_by_ids : t -> int Sequence.t -> t
end

module type TransitionLabel = sig
  type t
  (** A transition label consists of an unique id, an update function, a guard and a cost function. *)

  type update_element

  module Invariant = Guard

  val default : t
  (** Returns a default label with id 0, [true] as the guard,no update function and the default cost function. *)

  val update_map : t -> update_element VarMap.t
  (** Returns the update map of the transitionlabel *)

  val update : t -> Var.t -> update_element Option.t
  (** Returns the update of a variable. *)

  val equivalent : t -> t -> bool
  (** Returns if the two labels describe the same transition *)

  (* are both labels ids identical? *)
  val equal : t -> t -> bool

  val compare_equivalent : t -> t -> int
  (** TODO doc *)

  val compare : t -> t -> int
  (** Compare labels by ids *)

  val equivalent_update : t -> t -> bool

  val guard : t -> Guard.t
  (** Returns the guard of the label. *)

  val guard_without_inv : t -> Guard.t
  (** Returns the guard of the label without considering invariants. *)

  val invariant : t -> Invariant.t
  (** Returns the invariant. *)

  val id : t -> int
  (** Returns the unique id. *)

  val ids_to_string : ?pretty:bool -> t -> string
  (** Returns a string with the transitions id (or ids in case of probabilistic programs).
      Used for printing graphs. *)

  val cost : t -> Polynomials.Polynomial.t
  (** Returns the cost function *)

  val negative_costs : t -> bool

  val to_string : ?pretty:bool -> t -> string
  (** Returns a string representing the label. *)

  val update_to_string_lhs : t -> string
  (** Returns a string representing the left hand side of the update function. *)

  val update_to_string_rhs : t -> string
  (** Returns a string representing the right hand side of the update function. *)

  val update_to_string_lhs_pretty : t -> string
  val update_to_string_rhs_pretty : t -> string

  val cost_to_string : t -> string
  (** Returns a string representing the cost. *)

  val to_id_string : t -> string
  (** Returns a string representing the id of the label. *)

  val fill_up_arg_vars_up_to_num : int -> t -> t
  (** The call {i fill_up_arg_vars_up_to_num n} adds trivial updates for the first {i n}, i.e. Arg_0, .., Arg_n-1 arguments that are not contained in the labels update map *)

  (** TODO doc *)

  val vars : t -> VarSet.t
  (** Returns the set of variables. *)

  val input_vars : t -> VarSet.t
  (** Returns the set of input variables of the transition, i.e. the non temporary variables  *)

  val tmp_vars : t -> VarSet.t
  (** Returns the set of temporay variables of the transition.  *)

  val input_size : t -> int
  (** Returns the number of input variables *)

  val has_tmp_vars : t -> bool

  val changed_vars : t -> VarSet.t
  (** All input variables where the update is not x' = x.*)

  val chain_guards : t -> t -> Guard.t
  (** Guard that is true if both transitions can be executed one after another *)

  val remove_non_contributors : VarSet.t -> t -> t

  include Comparator.S with type t := t

  val sexp_of_t : t -> Sexp.t
end

module type ClassicalTransitionLabel = sig
  include TransitionLabel with type update_element = Polynomial.t

  val map_guard : (Guard.t -> Guard.t) -> t -> t
  (** Apply function to guard *)

  val relax_guard : non_static:VarSet.t -> t -> t
  (** Keeps only the atoms of the guard whose variables are a subset of non_static *)

  val overapprox_nonlinear_updates : t -> t
  (** Overapproximates nonlinear updates by nondeterministic updates. Useful for Farkas lemma *)

  (** Create an equivalent label with new id's, takes gt_id from the provided table and
      if not available creates and adds a new id_for the general transition to the table.
      *)
  (* val copy_rename: (int, int) Hashtbl.t -> t -> t *)
end

(** A transition connects two locations and is labeled with an updated function and a guard. *)
module type Transition = sig
  type transition_label
  type transition_label_comparator_witness

  type t = Location.t * transition_label * Location.t
  (** Type of a transition, i.e., two connected locations and a label. *)

  val equal : t -> t -> bool

  val compare : t -> t -> int
  (** compare transition ids *)

  val equivalent : t -> t -> bool
  val compare_equivalent : t -> t -> int

  val hash : t -> int
  (** Generates a hash value for a transition. *)

  val to_id_string : t -> string
  (** Returns a string of the form id: src -> target. *)

  val to_id_string_pretty : t -> string

  val to_string : t -> string
  (** Returns a string representing the transition. *)

  val to_string_pretty : t -> string

  val src : t -> Location.t
  (** Returns the source location of a transition. *)

  val label : t -> transition_label
  (** Returns the label of a transition. *)

  val map_label : (transition_label -> transition_label) -> t -> t
  (** Apply a function to a transitions label *)

  val target : t -> Location.t
  (** Returns the target location of a transition. *)

  val id : t -> int
  (** Returns an (unique) id of a transition label. TODO doc unique??*)

  val cost : t -> Polynomial.t
  (** Returns a cost function of a transition represented as a polynomial. *)

  include
    Comparator.S
      with type t := t
       and type comparator_witness =
        transition_label_comparator_witness TransitionComparator.comparator_witness

  val sexp_of_t : t -> Sexp.t
end

module type ClassicalTransition = sig
  include Transition

  val overapprox_nonlinear_updates : t -> t
end

(** This module represents a transition graph. *)
module type TransitionGraph = sig
  type transition_label
  type transition_label_comparator_witness
  type transition = Location.t * transition_label * Location.t

  type transition_comparator_witness =
    transition_label_comparator_witness TransitionComparator.comparator_witness

  type transition_set = (transition, transition_comparator_witness) Set.t

  include
    Graph.Sig.P
      with type V.t = Location.t
       and type V.label = Location.t
       and type E.t = transition
       and type E.label = transition_label

  val mk : transition Sequence.t -> t
  (** Creates a transition graph from an enum of transitions. *)

  val add_locations : Location.t Sequence.t -> t -> t
  (** Adds all locations from an enum to a transtion graph. *)

  val add_transitions : transition Sequence.t -> t -> t
  (** Adds all transitions from an enum to a transtion graph. Implicitly adds locations when they do not exit. *)

  val map_transitions : (transition -> transition) -> t -> t
  (** Apply function to the graphs transitions  *)

  val map_labels : (transition_label -> transition_label) -> t -> t
  (** Apply function to the graphs labels  *)

  val locations : t -> LocationSet.t
  (** Returns the set of locations. *)

  val transitions : t -> transition_set
  (** Returns the set of transitions. *)

  val loc_transitions : t -> Location.t list -> transition_set
  (** Returns the set of transitions consisting of transitions which are in the program graph and where both source and target location are part of the given location list. *)

  val add_invariant : Location.t -> Constraint.t -> t -> t
  (** Adds the invariant to a location of the program. *)

  val equivalent : t -> t -> bool
  (** Checks fore equivalence TODO: lies everywhere...*)

  val replace_edge_e : transition -> transition -> t -> t
  (** Replaces the first edge by the second edge. *)

  val sccs : t -> transition_set list
  (** Returns the (biggest) strongly connected components of the transiton graph. *)

  val sccs_from_sequence : transition Sequence.t -> transition_set list
  (** Returns the (biggest) strongly connected components of the transitons. *)
end

module type Program = sig
  type transition_label
  type transition_label_comparator_witness
  type transition = Location.t * transition_label * Location.t

  type transition_comparator_witness =
    transition_label_comparator_witness TransitionComparator.comparator_witness

  type transition_set = (transition, transition_comparator_witness) Set.t
  type transition_graph

  type t = (transition_label, transition_label_comparator_witness, transition_graph) GenericProgram_.t
  (** Type of a program consisting of a program graph and a start location. *)

  val transitions_from_location : t -> Location.t -> transition_set

  val remove_location : t -> Location.t -> t
  (** Removes the location from the program and all edges to it. *)

  val remove_unsatisfiable_transitions : t -> transition_set -> t
  (** Removes unsatisfiable transitions from a program.
      Note that for ProbabilisticPrograms this removes the whole general transitions as all general transitionsn share a guard *)

  val map_transitions : (transition -> transition) -> t -> t
  (** Apply function to the programs transitions  *)

  val map_labels : (transition_label -> transition_label) -> t -> t
  (** Apply function to the programs labels  *)

  val graph : t -> transition_graph
  (** Returns transition graph of a program. *)

  val add_invariant : Location.t -> Constraint.t -> t -> t
  (** Adds the invariant to a location of the program.
      The aim is to only store the atoms of the invariant that are not already contained in the guard *)

  val simplify_all_guards : t -> t
  (** Tries to simplify the guard of all transitions by invoking the SMT Solver *)

  val pre : t -> transition -> transition_set
  (** Returns a set of all transitions which occur directly before the given transition in the graph.
      Corresponds to pre(t).
      Note that the computation involves calls to the SMT solver and is therefore expensive.
      However, to improve performance they are cached inside the program type [t]. *)

  val pre_lazy : t -> transition -> transition Sequence.t
  (** Similar to pre but return the pre-transitions as a lazy Sequence.
      If the result has already been computed and cached in the value of type [t], then we do not recompute the pre-transitions.
      If however the pre-transitions haven't been computed yet then they are lazily computed when inspecting the returned sequence. These values are *not* added to the cached!
      This is for instance useful when cutting unsatisfiable transitions since during preprocessing the cache is invalidated many times and it suffices to check whether one pre-transition exists. *)

  val is_initial : t -> transition -> bool
  (** Returns true if the given transition is an initial transition. *)

  val is_initial_location : t -> Location.t -> bool
  (** Returns true if the given transition is an initial transition. *)

  val equivalent : t -> t -> bool
  (** Returns true if the program graphs are equivalent and both start locations are equal. *)

  val to_formatted_string : ?pretty:bool -> t -> FormattedString.t
  (** Returns a formatted string representing the program. *)

  val to_string : t -> string
  (** Returns a string representing the program. *)

  val to_simple_string : t -> string
  (** Input is not interpreted as a filepath, but as a program in simple mode. Method returns a string representation of a program from such an input. *)

  val vars : t -> VarSet.t
  (** Returns all variables of the program. *)

  val input_vars : t -> VarSet.t
  (** Returns all input variables of the program. *)

  val tmp_vars : t -> VarSet.t
  (** Returns the set of temporay variables of the transition. *)

  val locations : t -> LocationSet.t
  (** Returns all locations which occur in the transitions, but each location only once. *)

  val transitions : t -> transition_set
  (** Returns a set of all transitions which occur in the program graph of the program. *)

  val start : t -> Location.t
  (** Returns start location. *)

  val sccs : t -> transition_set List.t
  (** Returns the (biggest) strongly connected components of the transiton graph in topological order. *)

  val parallel_transitions : t -> transition -> transition_set
  (** Returns all transitions which are parallel to a given transition. Thus, all transitions start in the same location and end in the same location. *)

  val non_trivial_transitions : t -> transition_set
  (** Returns all transitions, that belong to an SCC. *)

  val entry_transitions : t -> transition list -> transition Batteries.List.t
  (** Computes all entry transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)

  val entry_transitions_with_logger :
    Batteries.Logger.log -> t -> transition list -> transition Batteries.List.t
  (** Like [entry_transitions] but loggs the results using the given logger *)

  val outgoing_transitions : Batteries.Logger.log -> t -> transition list -> transition Batteries.List.t
  (** Computes all outgoing transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)

  val remove_non_contributors : VarSet.t -> t -> t
  (** Remove the given variables that do not contribute to the problem *)

  (** This module exposes internal structure.
      Useful for tests. *)
  module InternalTest : sig
    val get_pre_cache : t -> (transition, transition_set) Hashtbl.t
    (** Obtain the internal cache of pre transitions *)

    val compute_pre : t -> transition -> transition Sequence.t
    (** Compute pre transitions by ignoring the cache. *)
  end
end

module type RV = sig
  type transition
  type transition_comparator_witness
  type t = transition * Var.t

  include
    Comparator.S
      with type t := t
       and type comparator_witness =
        (transition_comparator_witness, Var.comparator_witness) RVComparator.comparator_witness

  val transition : t -> transition
  (** Returns the transition of the result variable. *)

  val variable : t -> Var.t
  (** Returns the variable of the result variable. *)

  val to_id_string : t -> string
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
  val ids_to_string : ?pretty:bool -> t -> string
  val sexp_of_t : t -> Sexp.t
end

type !'a program_modules_meta

module type ProgramModules = sig
  module UpdateElement : PolyTypes.Polynomial with type value = OurInt.t
  module TransitionLabel : TransitionLabel with type update_element = UpdateElement.t

  module Transition :
    Transition
      with type transition_label = TransitionLabel.t
       and type transition_label_comparator_witness = TransitionLabel.comparator_witness

  module TransitionSet :
    TransitionSet
      with type elt = Transition.t
       and type elt_comparator_witness =
        TransitionLabel.comparator_witness TransitionComparator.comparator_witness

  module TransitionGraph :
    TransitionGraph
      with type transition_label = TransitionLabel.t
       and type transition_label_comparator_witness = TransitionLabel.comparator_witness
       and type transition = Transition.t

  module Program :
    Program
      with type transition_label = TransitionLabel.t
       and type transition_label_comparator_witness = TransitionLabel.comparator_witness
       and type transition = Transition.t
       and type transition_graph = TransitionGraph.t

  module RV : RV

  type program_modules_t =
    (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t) program_modules_meta
end

(** For classical/non-probabilistic programs we want polynomial updates only and RV.transition = Transition.t *)
module type ClassicalProgramModules = sig
  (* Can we avoid copy/pasting below? *)

  module UpdateElement : module type of Polynomial
  module TransitionLabel : ClassicalTransitionLabel

  module Transition :
    ClassicalTransition
      with type transition_label = TransitionLabel.t
       and type transition_label_comparator_witness = TransitionLabel.comparator_witness

  module TransitionSet :
    TransitionSet
      with type elt = Transition.t
       and type elt_comparator_witness =
        TransitionLabel.comparator_witness TransitionComparator.comparator_witness

  module TransitionGraph :
    TransitionGraph
      with type transition_label = TransitionLabel.t
       and type transition_label_comparator_witness = TransitionLabel.comparator_witness
       and type transition = Transition.t

  module Program :
    Program
      with type transition_label = TransitionLabel.t
       and type transition_label_comparator_witness = TransitionLabel.comparator_witness
       and type transition = Transition.t
       and type transition_graph = TransitionGraph.t

  module RV :
    RV
      with type transition = Transition.t
       and type transition_comparator_witness = Transition.comparator_witness

  type program_modules_t =
    (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t) program_modules_meta
end
