open! OurBase
(** Implementation of approximations containing time, size and cost-bounds. *)

type ('trans_appr_type, 'size_appr_type) approximation_t

(** Provides default implementations of an approximation *)
module Make
    (B : BoundType.Bound)
    (PM : ProgramTypes.ProgramModules)
    (T : ApproximationTypes.ApproximableTransitionType with type program = PM.Program.t)
    (TransitionApproximation :
      ApproximationTypes.TransitionApproximationType
        with type program = PM.Program.t
         and type bound = B.t
         and type transition = T.t)
    (SizeApproximation : ApproximationTypes.SizeApproximationType with type bound = B.t and type rv = PM.RV.t) : sig
  type t = (TransitionApproximation.t, SizeApproximation.t) approximation_t

  val empty : t
  (** Returns an empty approximation that does not contain any non-trivial information.
      That means, that every upper bound is infinite and every lower bound is minus infinite.
      The first parameter should be the count of transitions in the program.
      The second parameter should be the count of program variables. *)

  val filter_transitions_and_rvs : (T.t -> bool) -> (PM.RV.t -> bool) -> t -> t

  val to_formatted :
    ?show_initial:bool -> ?pretty:bool -> ?termination_only:bool -> PM.Program.t -> t -> FormattedString.t
  (**  Creates a formatted string containing time,size and cost-bounds. *)

  val to_string : ?show_initial:bool -> ?pretty:bool -> ?termination_only:bool -> PM.Program.t -> t -> string
  (**  Creates a string containing time,size and cost-bounds. *)

  (** {1  {L Timebound related methods}} *)

  val timebound : t -> T.t -> B.t
  (** Returns a timebound for the transition. *)

  val all_finite_timebounds : t -> (T.t * B.t) Sequence.t
  (** Returns a sequence of all registered finite time bounds *)

  val program_timebound : t -> PM.Program.t -> B.t
  (** Returns a timebound for the program. *)

  val add_timebound : B.t -> T.t -> t -> t
  (** Adds the information that the specified bound is a valid timebound for the given transition.
      The resulting approximation is guaranteed to be at least as good as the old approximation. *)

  val all_times_bounded : t -> T.t Sequence.t -> bool
  (** Returns true iff. all transitions from a given list of transitions are bounded and not infinity. *)

  val is_time_bounded : t -> T.t -> bool
  (** Returns true iff. a given transition is bounded and not infinity. *)

  (** {1  {L  Costbound related methods}} *)

  val costbound : t -> T.t -> B.t
  (** Returns a costbound for the transition. *)

  val all_finite_costbounds : t -> (T.t * B.t) Sequence.t
  (** Returns a sequence of all registered finite cost bounds *)

  val program_costbound : t -> PM.Program.t -> B.t
  (** Returns a costbound for the program. *)

  val add_costbound : B.t -> T.t -> t -> t
  (** Adds a (cost-)bound of a transition to an existing approximation. *)

  (** {1  {L  Sizebound related methods}} *)

  val sizebound : t -> PM.RV.transition -> Var.t -> B.t
  (** Returns a sizebound for the var of the transition.
          A sizebound is expressed in relation to the input variable values of the program. *)

  val all_finite_sizebounds : t -> (PM.RV.t * B.t) Sequence.t
  (** Returns a sequence of all registered finite cost bounds *)

  val add_sizebound : B.t -> PM.RV.transition -> Var.t -> t -> t
  (** Adds the information that the specified bound is a valid sizebound for the given variable of the transition.
          The resulting approximation is guaranteed to be at least as good as the old approximation. *)

  val add_sizebounds : B.t -> PM.RV.t list -> t -> t
  (** Add a size bound for all result variables of the list *)

  val is_size_bounded : PM.Program.t -> t -> PM.RV.transition -> bool
  (** Returns true iff. all size bounds of a given transition are bounded and not infinity. *)
end

module MakeForClassicalAnalysis (B : BoundType.Bound) (PM : ProgramTypes.ProgramModules) :
    module type of
      Make (B) (PM) (TransitionApproximation.MakeDefaultApproximableTransition (PM))
        (TransitionApproximation.Make (B) (TransitionApproximation.MakeDefaultApproximableTransition (PM)))
        (SizeApproximation.Make (B) (PM.RV))

include module type of MakeForClassicalAnalysis (Bounds.Bound) (ProgramModules)

module Probabilistic (BP : BoundPair.T) : sig
  module NonProbOverapprApproximation :
      module type of MakeForClassicalAnalysis (BP.ClassBound) (ProbabilisticProgramModules.NonProbOverappr)

  module ClassicalApproximation :
      module type of MakeForClassicalAnalysis (BP.ClassBound) (ProbabilisticProgramModules)

  module ExpApproximation :
      module type of
        Make
          (BP.ProbBound)
          (struct
            include ProbabilisticProgramModules
            module RV = GRV
          end)
          (TransitionApproximation.ApproximableGeneralTransition)
          (TransitionApproximation.Make (BP.ProbBound) (TransitionApproximation.ApproximableGeneralTransition))
          (SizeApproximation.Make (BP.ProbBound) (ProbabilisticProgramModules.GRV))

  val coerce_from_nonprob_overappr_approximation : NonProbOverapprApproximation.t -> ClassicalApproximation.t
  val coerce_from_classical_approximation : ClassicalApproximation.t -> NonProbOverapprApproximation.t

  type apprs = { appr : ExpApproximation.t; class_appr : ClassicalApproximation.t }
end
