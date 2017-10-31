open Batteries

(** Provides all module types related to preprocessors *)

type subject = Program.t * Approximation.t

type preprocessor =
  | CutUnreachableLocations
  | TrivialTimeBounds
  | CutUnsatisfiableTransitions
  | Chaining [@@deriving show, ord, eq]

val all_preprocessors : preprocessor list
  
(** Returns all the preprocessors that might successfully run after a run of the specific preprocessor. *)
val affects : preprocessor -> preprocessor list

(** Transforms a preprocessing step with the specific preprocessor on the subject.
    Results in a subject that might be changed. *)
val transform : subject -> preprocessor -> subject MaybeChanged.t

type strategy = preprocessor list -> subject -> subject

val all_strategies : strategy list
              
(** Uses the strategy to preprocess the given subject with the specified preprocessors. *)
val process : strategy -> preprocessor list -> subject -> subject
              
(** Applies each preprocessor exactly one time on the subject. *)
val process_only_once : strategy

(** Applies the preprocessors continously until a fixpoint is reached, such that no preprocessor is able to do another successful preprocessing step. *)
val process_til_fixpoint : strategy
