open Batteries
open Polynomials
open Constraints

(** A location is a node of a transition system and can be connected to other locations via transitions *)
module Location :
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
  val of_string : string -> t
end
module LocationSet :
sig
  include module type of Set.Make(Location)
  val to_string: t -> string
end

module Transition :
sig
  type t = Location.t * TransitionLabel.t * Location.t
  val same : t -> t -> bool
  val equivalent : t -> t -> bool
  val compare_same : t -> t -> int
  val compare_equivalent : t -> t -> int
  val hash : t -> int
  val to_id_string : t -> string
  val to_string : t -> string
  val src : t -> Location.t
  val label : t -> TransitionLabel.t
  val target : t -> Location.t
  val id : t -> int
  val cost : t -> Polynomial.t
  (** Adds the invariant to this transition. *)
  val add_invariant : Constraint.t -> t -> t
  val rename : Var.t list -> t -> t
end

module TransitionSet :
sig
  include module type of Set.Make(struct include Transition let compare = Transition.compare_same end)
  val powerset : t -> t Enum.t
  val to_string : t -> string
  val total_probability : t -> OurFloat.t
end

module GeneralTransition :
sig
  type t
  val of_transitionset: TransitionSet.t -> Transition.t -> t
  val compare: t -> t -> int
  val to_string: t -> string
  val to_id_string : t -> string
  val transitions: t -> TransitionSet.t
  val start: t -> Location.t
  val targets : t-> LocationSet.t
  val same : t -> t -> bool
  val id: t -> int
  val guard: t -> TransitionLabel.Guard.t
  val invariants: t -> TransitionLabel.Guard.t
  val guard_without_invariants: t -> TransitionLabel.Guard.t
  val total_probability: t -> OurFloat.t
  val input_vars: t -> VarSet.t
  val vars: t -> VarSet.t
  (** returns true iff all transitions in the general transition are loops *)
  val is_loop: t -> bool
  val target_string: t -> string
end

module GeneralTransitionSet :
sig
  include module type of Set.Make(struct include GeneralTransition let compare = GeneralTransition.compare end)
  val to_string: t -> string
  val to_id_string : t -> string
  val start_locations: t -> LocationSet.t
  val target_locations: t -> LocationSet.t
  val of_transitionset: TransitionSet.t -> t
end

module TransitionGraph :
sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(struct include TransitionLabel let compare = compare_same end)
  val locations : t -> LocationSet.t
  val transitions : t -> TransitionSet.t
  val generalized_transitions : t -> GeneralTransitionSet.t
  val loc_transitions : t -> Location.t list -> TransitionSet.t
  val equivalent : t -> t -> bool
  (** Replaces the first edge by the second edge. *)
  val replace_edge_e : Transition.t -> Transition.t -> t -> t
  (** Adds the invariant to the location of the graph. *)
  val add_invariant : Location.t -> Constraint.t -> t -> t
  val to_string: t -> string
end
