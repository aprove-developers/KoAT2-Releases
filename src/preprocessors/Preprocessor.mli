open Batteries
open ProgramTypes

(** Provides all module types related to preprocessors *)

type subject = Program.t * Approximation.t

type t =
  | CutUnreachableLocations
  | CutUnsatisfiableTransitions
  | Chaining
  | CutZeroProbTransitions
  | InvariantGeneration [@@deriving ord, eq]

val generate_invariants : Program.t -> Program.t

val show : t -> string

val all : t list

(** Returns all the preprocessors that might successfully run after a run of the specific preprocessor. *)
val affects : t -> t list

(** Transforms a preprocessing step with the specific preprocessor on the subject.
    Results in a subject that might be changed. *)
val transform : TransitionLabel.trans_id_counter -> subject -> t -> subject MaybeChanged.t

type strategy = TransitionLabel.trans_id_counter -> t list -> subject -> subject

val all_strategies : strategy list

(** Uses the strategy to preprocess the given subject with the specified preprocessors. *)
val process : TransitionLabel.trans_id_counter -> strategy -> t list -> subject -> subject

(** Applies each preprocessor exactly one time on the subject. *)
val process_only_once : strategy

(** Applies the preprocessors continously until a fixpoint is reached, such that no preprocessor is able to do another successful preprocessing step. *)
val process_til_fixpoint : strategy

val lift_to_program : (TransitionGraph.t -> TransitionGraph.t MaybeChanged.t) -> Program.t -> Program.t MaybeChanged.t

val lift_to_tuple : ('b -> 'c MaybeChanged.t) -> ('b * 'a) -> ('c * 'a) MaybeChanged.t
