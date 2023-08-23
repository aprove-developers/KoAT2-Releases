open! OurBase
open ProbabilisticProgramModules
open Bounds

(** This module contains useful functions for the computation of probabilistic bounds. *)

val entry_gts_with_locs : Program.t -> GeneralTransitionSet.t -> (GeneralTransition.t * Location.t) Sequence.t
(** Obtain all entry general transition to the general transition set with corresponding entry locations *)

val entry_gts : Program.t -> GeneralTransitionSet.t -> GeneralTransitionSet.t
(** Obtain all entry general transition to the general transition set *)

val entry_locations : Program.t -> GeneralTransitionSet.t -> LocationSet.t
(** Obtain all entry locations of the general transition set *)

(** Helper module to substitute variable in bounds with their expected/classical sizes.
    We use a (sound) heuristic to substitute variables with their expected sizes whenever possible *)
module SubstHelper : sig
  val substitute_bound_with_exp_and_class_sizes :
    exp_subst:(Var.t -> Bound.t) -> class_subst:(Var.t -> Bound.t) -> Bound.t -> Bound.t
  (** Substitutes a bound with the given expected and classical size bounds *)

  val substitute_bound_with_exp_and_class_sizes_get_size :
    exp_subst:(Var.t -> Bound.t) -> class_subst:(Var.t -> Bound.t) -> Bound.t -> Var.t -> Bound.t
  (** Similar to [substitute_bound_with_exp_and_class_sizes] but instead returns a function that assigns a variable the size bound that should be used in the given argument bound.
      Useful when dealing with [UnliftedBounds] *)
end

(** Like [SubstHelper] but for rational bounds. *)
module RationalSubstHelper : sig
  val substitute_bound_with_exp_and_class_sizes :
    exp_subst:(Var.t -> RationalBound.t) ->
    class_subst:(Var.t -> RationalBound.t) ->
    RationalBound.t ->
    RationalBound.t

  val substitute_bound_with_exp_and_class_sizes_get_size :
    exp_subst:(Var.t -> RationalBound.t) ->
    class_subst:(Var.t -> RationalBound.t) ->
    RationalBound.t ->
    Var.t ->
    RationalBound.t
end
