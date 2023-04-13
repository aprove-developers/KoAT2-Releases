(** Implementation of Multiphase Ranking Functions. *)
open Batteries
open Constraints
open Atoms
open Polynomials
open ProgramModules
open BoundsInst

(** Implementation of multiphase ranking function based on Linear Ranking Functions, flag --mrf has to be set to use multiphase ranking function. *)

(** Type of measurement of ranking function, i.e., cost or time. *)
type measure = [ `Cost | `Time ] [@@deriving show]

module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  (** Type of ranking function consisting of a function mapping from locations to lists of polynomials, a decreasing transition, a set of non-increasing transitions and the depth of the ranking function. *)
  type t

  (** Returns a list of polynomials representing a multiphase ranking function. *)
  val rank : t -> (PM.Location.t -> Polynomial.t) list

  (** Returns a non-empty list of all transitions which are strictly decreasing and at the same time bounded with one.
      Corresponds to T_> . *)
  val decreasing : t -> PM.Transition.t

  (** Returns a list of all transitions for which the multiphase ranking function is defined.
      Corresponds to T'. *)
  val non_increasing : t -> PM.TransitionSet.t

  (** Returns the depth of a multiphase ranking function (i.e. returns d if MRF has form f1,f2,...,fd).*)
  val depth : t -> int

  (** Tries to find a suitable multiphase ranking function for the given transitions T'.
  * The int corresponds to the maximum depth of the mprf *)
  val find : measure -> PM.Program.t -> int -> t Enum.t

  val find_scc : measure -> PM.Program.t ->
    (PM.Transition.t -> bool) ->  (* Is the transition time-bounded? *)
    (PM.Transition.t -> VarSet.t) -> (* Unbounded vars for the transition *)
    PM.TransitionSet.t ->  (* The scc*)
    int -> (* depth of the ranking function *)
    PM.Transition.t -> (* The transition that should be decreasing *)
    t option

  (** Converts a multiphase ranking function into a string*)
  val to_string : t -> string

  val only_rank_to_string : t -> string
  (** Converts a multiphase ranking function into a string without any further information. *)

  val add_to_proof : t -> Bound.t option -> PM.Program.t -> unit
end

include module type of Make(ProgramModules)
