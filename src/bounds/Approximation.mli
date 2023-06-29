(** Implementation of approximations containing time, size and cost-bounds. *)
open Batteries
open BoundsInst

(** Provides default implementations of an approximation *)

module Make(B: BoundType.Bound)(PM: ProgramTypes.ProgramModules)
           (T: TransitionApproximationType.ApproximableTransition with type program = PM.Program.t): sig
  (* module TransitionApproximation: module type of TransitionApproximationType.Make(B)(PM) *)
  (* module SizeApproximation: module type of SizeApproximationType.Make(B)(RV) *)
  type t

  (** Returns an empty approximation that does not contain any non-trivial information.
      That means, that every upper bound is infinite and every lower bound is minus infinite.
      The first parameter should be the count of transitions in the program.
      The second parameter should be the count of program variables. *)
  val empty : int -> int -> t

  (** Creates an empty approximation of a given program. *)
  val create : PM.Program.t -> t

  (**  Creates a formatted string containing time,size and cost-bounds. *)
  val to_formatted : ?show_initial:bool -> ?pretty:bool -> ?termination_only:bool -> PM.Program.t -> t -> FormattedString.t

  (**  Creates a string containing time,size and cost-bounds. *)
  val to_string : ?termination_only:bool -> PM.Program.t -> t -> string

  (** Returns true iff. time and size-bounds are equivalent. Costs-bounds are not considered. *)
  val equivalent : t -> t -> bool


  (** {1  {L Timebound related methods}} *)

  (** Returns a timebound for the transition. *)
  val timebound : t -> T.t -> B.t

  (** Returns a timebound for the transition id. *)
  val timebound_id : t -> int -> B.t

  (** Returns a timebound for the program. *)
  val program_timebound : t -> PM.Program.t -> B.t

  (** Adds the information that the specified bound is a valid timebound for the given transition.
      The resulting approximation is guaranteed to be at least as good as the old approximation. *)
  val add_timebound : B.t -> T.t -> t -> t

  (** Returns true iff. all transitions from a given list of transitions are bounded and not infinity. *)
  val all_times_bounded : t -> T.t Enum.t -> bool

  (** Returns true iff. a given transition is bounded and not infinity. *)
  val is_time_bounded : t -> T.t -> bool


  (** {1  {L  Costbound related methods}} *)

  (** Returns a costbound for the transition. *)
  val costbound : t -> T.t -> B.t

  (** Returns a costbound for the program. *)
  val program_costbound : t -> PM.Program.t -> B.t

  (** Adds a (cost-)bound of a transition to an existing approximation. *)
  val add_costbound : B.t -> T.t -> t -> t


  (** {1  {L  Sizebound related methods}} *)

  (** Returns a sizebound for the var of the transition.
          A sizebound is expressed in relation to the input variable values of the program. *)
  val sizebound : t -> PM.RV.transition -> Var.t -> B.t

  (** Adds the information that the specified bound is a valid sizebound for the given variable of the transition.
          The resulting approximation is guaranteed to be at least as good as the old approximation. *)
  val add_sizebound : B.t -> PM.RV.transition -> Var.t -> t -> t

  (** Add a size bound for all result variables of the list *)
  val add_sizebounds : B.t -> PM.RV.t list -> t -> t

  (** Returns true iff. all size bounds of a given transition are bounded and not infinity. *)
  val is_size_bounded : PM.Program.t -> t -> PM.RV.transition -> bool
end

module MakeWithDefaultTransition(B: BoundType.Bound)(PM: ProgramTypes.ProgramModules):
  module type of Make(B)(PM)(TransitionApproximationType.MakeDefaultApproximableTransition(PM))

module MakeForClassicalAnalysis(PM: ProgramTypes.ProgramModules):
  module type of MakeWithDefaultTransition(BoundsInst.Bound)(PM)


include module type of MakeForClassicalAnalysis(ProgramModules)


module Coerce(B: BoundType.Bound)
             (PM: ProgramTypes.ProgramModules)(PM': ProgramTypes.ProgramModules)
             (_: sig

                val t_eq: ( TransitionApproximationType.MakeDefaultApproximableTransition(PM).t
                          , TransitionApproximationType.MakeDefaultApproximableTransition(PM').t) Util.TypeEq.t

                module RVTupleEq: functor(F: functor(_: ProgramTypes.RVTuple) -> sig type t end) -> sig
                  val proof: (F(PM.RV.RVTuple_).t, F(PM'.RV.RVTuple_).t) Util.TypeEq.t
                 end
              end): sig

  val coerce: MakeWithDefaultTransition(B)(PM).t -> MakeWithDefaultTransition(B)(PM').t
end

module Probabilistic: sig
  module NonProbOverapprApproximation:
    module type of MakeForClassicalAnalysis(ProbabilisticProgramModules.NonProbOverappr)
  module ClassicalApproximation:
    module type of
      MakeWithDefaultTransition(BoundsInst.Bound)(ProbabilisticProgramModules)
  module ExpApproximation:
    module type of
      Make(BoundsInst.RealBound)
          (struct include ProbabilisticProgramModules module RV = GRV end)
          (struct
            open ProbabilisticProgramModules
            type program = Program.t
            include GeneralTransition
            let id = gt_id
            let all_from_program = GeneralTransitionSet.enum % Program.gts
          end)

  val coerce_from_nonprob_overappr_approximation: NonProbOverapprApproximation.t -> ClassicalApproximation.t
end
