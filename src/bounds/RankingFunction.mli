(** Implementation of (linear) Ranking Functions. *)
open Batteries
open Constraints
open Atoms
open Polynomials
open ProgramTypes

(** Provides default implementations of RankingFunctions. KoAT uses this as a default function if the user does not specify any requirements by setting parameters (e.g. --mprf). *)

(** Type of ranking function consisting of a function mapping from locations to polynomials, a decreasing transition and a set of non-increasing transitions. *)
type t

(** Type of measurement of ranking function, i.e., cost or time. *)
type measure = [ `Cost | `Time ] [@@deriving show]

type ranking_cache

val new_cache : unit -> ranking_cache

(** Returns the ranking polynomial for the specific location. *)
val rank : t -> Location.t -> Polynomial.t

(** Returns a non-empty list of all transitions which are strictly decreasing and at the same time bounded with one.
    Corresponds to T_> . *)
val decreasing : t -> Transition.t

(** Returns a list of all transitions for which the prf is defined.
    Corresponds to T'. *)
val non_increasing : t -> TransitionSet.t

(** Finds a suitable ranking function for the given transitions T'. *)
val find : ranking_cache -> ?inv:bool ->  measure -> bool -> Program.t -> Transition.t -> t list

(* The argument of type Transition.t -> bool is a function that returns true if the transition has a finite time bound.
 * It is used to compute 'good' non-increasing sets.
 * the argument of type Transition.t -> VarSet.t assigns a transition the variables that have an unbounded size for this
 * transition *)
val find_scc : ranking_cache -> ?inv:bool -> measure -> bool -> Program.t ->
  TransitionSet.t TransitionTable.t -> (* Map of transitions to the corresponding entry transitions *)
  Transition.t -> (Transition.t -> bool) -> (Transition.t -> VarSet.t) ->
  ProgramTypes.TransitionSet.t -> (* Transitions which should be decreasing in a ranking function. Use all transitions with unbounded time here *)
  ProgramTypes.TransitionSet.t ->  (* The scc*)
  t list

val find_fast : ranking_cache -> ?inv:bool ->  measure -> bool -> Program.t -> Transition.t -> t list

val find_scc_fast : ranking_cache -> ?inv:bool -> measure -> bool -> Program.t -> Transition.t -> ProgramTypes.TransitionSet.t -> t list

(** Converts a ranking function into a string*)
val to_string : t -> string

(** Converts a ranking function into a string without any further information. *)
val only_rank_to_string : t -> string

(** Resets all cached data.
    Useful for testing in the same OCaml instance. *)
val reset : ranking_cache -> unit
