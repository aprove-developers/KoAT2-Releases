(** Provides all module types related to preprocessors. *)
open Batteries
open ProgramModules

(** Type of preprocessor. *)
type _ t =
  | CutZeroProbTransitions: ProbabilisticPrograms.ProbabilisticProgram.t t
  | CutUnreachableLocations: 'p t  (** Removes all unreachable locations. *)
  | CutUnsatisfiableTransitions: 'p t  (** Removes all unsatisfiable transitions. *)
  | EliminateNonContributors: 'p t
  | Chaining: Program.t t (** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location, making the location obsolete. *)
  | InvariantGeneration: 'p t  (** Adds invariants to transitions. *)

(** Returns a string representing type of preprocessor. *)
val show : 'p t -> string

(** Returns all available classical preprocessors. *)
val all_classical : Program.t t list

(** Returns all available probabilistic preprocessors. *)
val all_probabilistic : ProbabilisticPrograms.ProbabilisticProgram.t t list

(** Returns all preprocessors that are not tied to a specific representation *)
val all_generic: 'a t list

(** Returns all the preprocessors that might successfully run after a run of the specific preprocessor. *)
val affects : 'p t -> 'p t list

(** Transforms a preprocessing step with the specific preprocessor on the Program.t.
    Results in a Program.t that might be changed. *)
val transform: 'p.
  (module ProgramTypes.ClassicalProgramModules with type Program.t = 'p) -> 'p -> 'p t -> 'p MaybeChanged.t

(** The type strategy is a list of preprocessors for a Program.t to derive a new Program.t. *)
type 'p strategy = (module ProgramTypes.ClassicalProgramModules with type Program.t = 'p) -> 'p t list -> 'p -> 'p

(** A list with [process_only_once] and [process_till_fixpoint]. *)
val all_strategies : 'p strategy list

(** Uses the strategy to preprocess the given Program.t with the specified preprocessors. *)
val process: 'p. (module ProgramTypes.ClassicalProgramModules with type Program.t = 'p) -> 'p strategy -> 'p t list -> 'p -> 'p

(** Applies each preprocessor exactly one time on the Program.t. *)
val process_only_once: 'p strategy

(** Applies the preprocessors continously until a fixpoint is reached, such that no preprocessor is able to do another successful preprocessing step. *)
val process_till_fixpoint: 'p strategy

(** TODO doc *)
val lift_to_program : (TransitionGraph.t -> TransitionGraph.t MaybeChanged.t) -> Program.t -> Program.t MaybeChanged.t
