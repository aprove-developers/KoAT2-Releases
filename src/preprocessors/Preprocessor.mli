(** Provides all module types related to preprocessors. *)
open Batteries
open ProgramTypes

(** Provides all module types related to preprocessors. *)

(** Type of preprocessor. *)
type t =
  | CutUnreachableLocations  (** Removes all unreachable locations. *)
  | CutUnsatisfiableTransitions  (** Removes all unsatisfiable transitions. *)
  | EliminateNonContributors
  | Chaining (** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location, making the location obsolete. *)
  | InvariantGeneration  [@@deriving ord, eq] (** Adds invariants to transitions. *)

(** Returns a string representing type of preprocessor. *)
val show : t -> string

(** Returns all available preprocessor. *)
val all : t list

(** Returns all the preprocessors that might successfully run after a run of the specific preprocessor. *)
val affects : t -> t list

(** Transforms a preprocessing step with the specific preprocessor on the Program.t.
    Results in a Program.t that might be changed. *)
val transform : Program.t -> t -> Program.t MaybeChanged.t

(** The type strategy is a list of preprocessors for a Program.t to derive a new Program.t. *)
type strategy = t list -> Program.t -> Program.t

(** A list with [process_only_once] and [process_til_fixpoint]. *)
val all_strategies : strategy list

(** Uses the strategy to preprocess the given Program.t with the specified preprocessors. *)
val process : strategy -> t list -> Program.t -> Program.t

(** Applies each preprocessor exactly one time on the Program.t. *)
val process_only_once : strategy

(** Applies the preprocessors continously until a fixpoint is reached, such that no preprocessor is able to do another successful preprocessing step. *)
val process_til_fixpoint : strategy

(** TODO doc *)
val lift_to_program : (TransitionGraph.t -> TransitionGraph.t MaybeChanged.t) -> Program.t -> Program.t MaybeChanged.t
