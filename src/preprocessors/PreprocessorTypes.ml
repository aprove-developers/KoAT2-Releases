open Batteries

type subject = Program.t * Approximation.t

type preprocessor =
  | CutUnreachableLocations
  | TrivialTimeBounds
  | CutUnsatisfiableTransitions
  | Chaining [@@deriving show, ord, eq]

let affects = function
  | CutUnreachableLocations -> []
  | TrivialTimeBounds -> []
  | CutUnsatisfiableTransitions -> [CutUnreachableLocations; Chaining]
  | Chaining -> [CutUnsatisfiableTransitions]

let transform subject = function
  | CutUnreachableLocations -> MaybeChanged.lift_to_subject CutUnreachableLocations.transform_program subject
  | TrivialTimeBounds -> TrivialTimeBounds.transform subject
  | CutUnsatisfiableTransitions -> MaybeChanged.lift_to_subject CutUnsatisfiableTransitions.transform_program subject
  | Chaining -> MaybeChanged.lift_to_subject (MaybeChanged.lift_to_program Chaining.transform_graph) subject
  
module PreprocessorSet =
  Set.Make(
      struct
        type t = preprocessor
        let compare = compare_preprocessor
      end
    )

let all_preprocessors =
  [CutUnreachableLocations; TrivialTimeBounds; CutUnsatisfiableTransitions; Chaining]
  
type strategy = preprocessor list -> subject -> subject

let process strategy preprocessors subject =
  strategy preprocessors subject

let process_only_once preprocessors =
  PreprocessorSet.fold (fun preprocessor subject -> MaybeChanged.unpack (transform subject preprocessor)) (PreprocessorSet.of_list preprocessors)

let rec process_til_fixpoint_ ?(wanted=PreprocessorSet.of_list all_preprocessors) (todos: PreprocessorSet.t) (subject: subject) : subject =
  if PreprocessorSet.is_empty todos then
    subject
  else
    let (preprocessor, others) = PreprocessorSet.pop todos in
    let maybe_changed = transform subject preprocessor in
    let new_preprocessor_set =
      if MaybeChanged.has_changed maybe_changed then
        PreprocessorSet.(preprocessor |> affects |> of_list |> inter wanted |> union others)
      else others in
    process_til_fixpoint_ ~wanted new_preprocessor_set (MaybeChanged.unpack maybe_changed)

let process_til_fixpoint preprocessors =
  let set = PreprocessorSet.of_list preprocessors in
  process_til_fixpoint_ ~wanted:set set
  
let all_strategies = [process_only_once; process_til_fixpoint]
  
