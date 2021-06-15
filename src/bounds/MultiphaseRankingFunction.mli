(** Implementation of Multiphase Ranking Functions. *)
open Batteries
open Constraints
open Atoms
open Polynomials
open ProgramTypes

(** Implementation of multiphase ranking function based on Linear Ranking Functions, flag --mrf has to be set to use multiphase ranking function. *)

(** Type of ranking function consisting of a function mapping from locations to lists of polynomials, a decreasing transition, a set of non-increasing transitions and the depth of the ranking function. *)
type t

(** Type of measurement of ranking function, i.e., cost or time. *)
type measure = [ `Cost | `Time ] [@@deriving show]

type ranking_cache

(** Returns a list of polynomials representing a multiphase ranking function. *)
val rank : t -> (Location.t -> Polynomial.t) list

(** Returns a non-empty list of all transitions which are strictly decreasing and at the same time bounded with one.
    Corresponds to T_> . *)
val decreasing : t -> Transition.t

(** Returns a list of all transitions for which the multiphase ranking function is defined.
    Corresponds to T'. *)
val non_increasing : t -> TransitionSet.t

(** Returns the depth of a multiphase ranking function (i.e. returns d if MRF has form f1,f2,...,fd).*)
val depth : t -> int

(** A reference to store the maximum depth to bound the search space of multiphase ranking function. The default value is 5 and can be adjusted by the user with flag -d. *)
val maxDepth : int Batteries.ref

(** Finds a suitable multiphase ranking function for the given transitions T'. *)
val find : ranking_cache -> ?inv:bool -> measure -> bool -> Program.t -> Transition.t -> t list

val find_scc : ranking_cache -> ?inv:bool -> measure -> bool ->  Program.t ->
  TransitionSet.t TransitionTable.t -> (* Map of transitions to the corresponding entry transitions *)
  Transition.t -> (Transition.t -> bool) -> (Transition.t -> VarSet.t) ->
  ProgramTypes.TransitionSet.t -> (* Transitions which should be decreasing in a ranking function. Use all transitions with unbounded time here *)
  ProgramTypes.TransitionSet.t ->  (* The scc*)
  t list

val find_fast : ranking_cache -> ?inv:bool ->  measure -> bool -> Program.t -> Transition.t -> t list

val find_scc_fast : ranking_cache -> ?inv:bool -> measure -> bool -> Program.t -> Transition.t -> ProgramTypes.TransitionSet.t -> t list

(** Converts a multiphase ranking function into a string*)
val to_string : t -> string

val only_rank_to_string : t -> string
(** Converts a multiphase ranking function into a string without any further information. *)

(** Resets all cached data.
    Useful for testing in the same OCaml instance. *)
val reset : ranking_cache -> unit

val new_cache : unit -> ranking_cache
