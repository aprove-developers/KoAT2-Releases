open OurBase
(** Implementation of approximations containing time, size and cost-bounds. *)

open Bounds

type ('trans, 'bound, 'rv, 'trans_cmp_wit, 'rv_comp_wit) approximation_t

(** Provides default implementations of an approximation *)
module Make
    (B : BoundType.Bound)
    (PM : ProgramTypes.ProgramModules)
    (T : TransitionApproximationType.ApproximableTransition with type program = PM.Program.t) : sig
  (* type t = (T.t,Bound.t,PM.RV.t,T.comparator_witness,PM.RV.RVTuple_.comparator_witness) approximation_t *)
  type t = (T.t, B.t, PM.RV.t, T.comparator_witness, PM.RV.comparator_witness) approximation_t

  val empty : t
  (** Returns an empty approximation that does not contain any non-trivial information.
      That means, that every upper bound is infinite and every lower bound is minus infinite.
      The first parameter should be the count of transitions in the program.
      The second parameter should be the count of program variables. *)

  val to_formatted :
    ?show_initial:bool -> ?pretty:bool -> ?termination_only:bool -> PM.Program.t -> t -> FormattedString.t
  (**  Creates a formatted string containing time,size and cost-bounds. *)

  val to_string : ?show_initial:bool -> ?pretty:bool -> ?termination_only:bool -> PM.Program.t -> t -> string
  (**  Creates a string containing time,size and cost-bounds. *)

  (** {1  {L Timebound related methods}} *)

  val timebound : t -> T.t -> B.t
  (** Returns a timebound for the transition. *)

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

  val program_costbound : t -> PM.Program.t -> B.t
  (** Returns a costbound for the program. *)

  val add_costbound : B.t -> T.t -> t -> t
  (** Adds a (cost-)bound of a transition to an existing approximation. *)

  (** {1  {L  Sizebound related methods}} *)

  val sizebound : t -> PM.RV.transition -> Var.t -> B.t
  (** Returns a sizebound for the var of the transition.
          A sizebound is expressed in relation to the input variable values of the program. *)

  val add_sizebound : B.t -> PM.RV.transition -> Var.t -> t -> t
  (** Adds the information that the specified bound is a valid sizebound for the given variable of the transition.
          The resulting approximation is guaranteed to be at least as good as the old approximation. *)

  val add_sizebounds : B.t -> PM.RV.t list -> t -> t
  (** Add a size bound for all result variables of the list *)

  val is_size_bounded : PM.Program.t -> t -> PM.RV.transition -> bool
  (** Returns true iff. all size bounds of a given transition are bounded and not infinity. *)
end

module MakeWithDefaultTransition (B : BoundType.Bound) (PM : ProgramTypes.ProgramModules) :
    module type of Make (B) (PM) (TransitionApproximationType.MakeDefaultApproximableTransition (PM))

module MakeForClassicalAnalysis (PM : ProgramTypes.ProgramModules) :
    module type of MakeWithDefaultTransition (Bounds.Bound) (PM)

include module type of MakeForClassicalAnalysis (ProgramModules)

module Probabilistic : sig
  module NonProbOverapprApproximation :
      module type of MakeForClassicalAnalysis (ProbabilisticProgramModules.NonProbOverappr)

  module ClassicalApproximation :
      module type of MakeWithDefaultTransition (Bounds.Bound) (ProbabilisticProgramModules)

  module ExpApproximation :
      module type of
        Make
          (Bounds.RealBound)
          (struct
            include ProbabilisticProgramModules
            module RV = GRV
          end)
          (struct
            open ProbabilisticProgramModules

            type program = Program.t

            include GeneralTransition

            let id = gt_id
            let all_from_program = Set.to_sequence % Program.gts
          end)

  val coerce_from_nonprob_overappr_approximation : NonProbOverapprApproximation.t -> ClassicalApproximation.t
end
