open! OurBase
(** Provides all module types related to preprocessors. *)

open ProgramModules

(** Type of preprocessor. *)
type _ t =
  | CutZeroProbTransitions : ProbabilisticPrograms.ProbabilisticProgram.t t
  | CutUnreachableLocations : 'p t  (** Removes all unreachable locations. *)
  | CutUnsatisfiableTransitions : 'p t  (** Removes all unsatisfiable transitions. *)
  | EliminateNonContributors : 'p t
  | EliminateTempVars : Program.t t
  | Chaining : Program.t t
      (** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location, making the location obsolete. *)
  | ChainingConservative : Program.t t
      (** Same as chaining but only for locations with one entry and one outgoing edge. *)
  | InvariantGeneration : 'p t  (** Adds invariants to transitions. *)

val show : 'p t -> string
(** Returns a string representing type of preprocessor. *)

val all_classical : Program.t t list
(** Returns all available classical preprocessors. *)

val all_probabilistic : ProbabilisticPrograms.ProbabilisticProgram.t t list
(** Returns all available probabilistic preprocessors. *)

val all_generic : 'a t list
(** Returns all preprocessors that are not tied to a specific representation *)

val affects : 'p t -> 'p t list
(** Returns all the preprocessors that might successfully run after a run of the specific preprocessor. *)

type strategy
(** A strategy determines how the preprocessors will be run on a program *)

val process_only_once : strategy
(** Applies each preprocessor exactly one time on the Program.t. *)

val process_till_fixpoint : strategy
(** Applies the preprocessors continously until a fixpoint is reached, such that no preprocessor is able to do another successful preprocessing step. *)

val all_strategies : strategy list
(** A list with [process_only_once] and [process_till_fixpoint]. *)

val lift_to_program :
  (TransitionGraph.t -> TransitionGraph.t MaybeChanged.t) -> Program.t -> Program.t MaybeChanged.t
(** Helper function for tests *)

(** This functor allows to preprocess a program using the provided program modules.
    The program will both be processes using PM and CPM (e.g. for Invariant Generations).
    The last module contains a proof that both program types are equal. *)
module Make
    (PM : ProgramTypes.ProgramModules)
    (CPM : ProgramTypes.ClassicalProgramModules)
    (_ : sig
      val eq : (PM.Program.t, CPM.Program.t) Type_equal.t
    end) : sig
  val process : strategy -> PM.Program.t t list -> PM.Program.t -> PM.Program.t
  (** Uses the strategy to preprocess the given Program.t with the specified preprocessors. *)
end

(** This is a convenience functor which instantiates Make for classical program modules only.*)
module MakeForClassicalProgramModules (CPM : ProgramTypes.ClassicalProgramModules) : sig
  val process : strategy -> CPM.Program.t t list -> CPM.Program.t -> CPM.Program.t
  (** Uses the strategy to preprocess the given Program.t with the specified preprocessors. *)
end

module StandardProgram : module type of MakeForClassicalProgramModules (ProgramModules)

module ProbabilisticWithOverappr :
    module type of
      Make (ProbabilisticProgramModules) (ProbabilisticProgramModules.NonProbOverappr)
        (struct
          let eq = ProbabilisticPrograms.Equalities.program_equalities
        end)
