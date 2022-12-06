(** Implementation of approximations containing time, size and cost-bounds. *)
open Batteries
open BoundsInst

(** Provides default implementations of an approximation *)

module Make(PM: ProgramTypes.ProgramModules): sig
  module TransitionApproximation: module type of TransitionApproximationType.Make(Bound)(PM)
  module SizeApproximation: module type of SizeApproximationType.Make(Bound)(RVGTypes.MakeRV(PM.TransitionLabel)(PM.Transition))
  type t

  (** Returns an empty approximation that does not contain any non-trivial information.
      That means, that every upper bound is infinite and every lower bound is minus infinite.
      The first parameter should be the count of transitions in the program.
      The second parameter should be the count of program variables. *)
  val empty : int -> int -> t

  (** Creates an empty approximation of a given program. *)
  val create : PM.Program.t -> t

  (** Returns time-approximation. *)
  val time : t -> TransitionApproximation.t

  (** Returns size-approximation. *)
  val size : t -> SizeApproximation.t

  (** Returns cost-approximation. *)
  val cost : t -> TransitionApproximation.t


  (**  Creates a formatted string containing time,size and cost-bounds. *)
  val to_formatted : ?show_initial:bool -> ?pretty:bool -> PM.Program.t -> t -> FormattedString.t

  (**  Creates a string containing time,size and cost-bounds. *)
  val to_string : PM.Program.t -> t -> string

  (** Returns true iff. time and size-bounds are equivalent. Costs-bounds are not considered. *)
  val equivalent : t -> t -> bool


  (** {1  {L Timebound related methods}} *)

  (** Returns a timebound for the transition. *)
  val timebound : t -> PM.Transition.t -> Bound.t

  (** Returns a timebound for the transition id. *)
  val timebound_id : t -> int -> Bound.t

  (** Returns a timebound for the program. *)
  val program_timebound : t -> PM.Program.t -> Bound.t

  (** Adds the information that the specified bound is a valid timebound for the given transition.
      The resulting approximation is guaranteed to be at least as good as the old approximation. *)
  val add_timebound : Bound.t -> PM.Transition.t -> t -> t

  (** Returns true iff. all transitions from a given list of transitions are bounded and not infinity. *)
  val all_times_bounded : t -> PM.Transition.t list -> bool

  (** Returns true iff. a given transition is bounded and not infinity. *)
  val is_time_bounded : t -> PM.Transition.t -> bool


  (** {1  {L  Costbound related methods}} *)

  (** Returns a costbound for the transition. *)
  val costbound : t -> PM.Transition.t -> Bound.t

  (** Returns a costbound for the program. *)
  val program_costbound : t -> PM.Program.t -> Bound.t

  (** Adds a (cost-)bound of a transition to an existing approximation. *)
  val add_costbound : Bound.t -> PM.Transition.t -> t -> t


  (** {1  {L  Sizebound related methods}} *)

  (** Returns a sizebound for the var of the transition.
          A sizebound is expressed in relation to the input variable values of the program. *)
  val sizebound : t -> PM.Transition.t -> Var.t -> Bound.t

  (** Adds the information that the specified bound is a valid sizebound for the given variable of the transition.
          The resulting approximation is guaranteed to be at least as good as the old approximation. *)
  val add_sizebound : Bound.t -> PM.Transition.t -> Var.t -> t -> t

  (** Add a size bound for all result variables of the list *)
  val add_sizebounds : Bound.t -> RVGTypes.MakeRV(PM.TransitionLabel)(PM.Transition).t list -> t -> t

  (** Returns true iff. all size bounds of a given transition are bounded and not infinity. *)
  val is_size_bounded : PM.Program.t -> t -> PM.Transition.t -> bool

  val min : PM.Program.t -> t -> t -> t
end

include module type of Make(ProgramModules)
