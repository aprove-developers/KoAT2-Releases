open Batteries

let logger = Logging.(get Preprocessor)

type subject = Program.t * Approximation.t

type t =
  | CutUnreachableLocations
  | CutUnsatisfiableTransitions
  | Chaining
  | CutZeroProbTransitions
  | InvariantGeneration[@@deriving ord, eq]

let show = function
  | CutUnreachableLocations -> "reachable"
  | CutUnsatisfiableTransitions -> "sat"
  | Chaining -> "chaining"
  | InvariantGeneration -> "invgen"
  | CutZeroProbTransitions -> "zerotransitions"

let affects = function
  | CutUnreachableLocations     -> []
  | InvariantGeneration         -> [CutUnsatisfiableTransitions]
  | CutUnsatisfiableTransitions -> [CutUnreachableLocations; Chaining]
  | Chaining                    -> [CutUnsatisfiableTransitions; Chaining; InvariantGeneration]
  | CutZeroProbTransitions      -> [CutUnreachableLocations; Chaining]

let lift_to_program transform program =
  MaybeChanged.(transform (Program.graph program) >>= (fun graph -> same (Program.map_graph (const graph) program)))

let lift_to_tuple transform tuple =
  MaybeChanged.(transform (Tuple2.first tuple) >>= (fun program -> same (Tuple2.map1 (const program) tuple)))

let transform subject = function
  | CutUnreachableLocations     -> lift_to_tuple CutUnreachableLocations.transform_program subject
  | CutUnsatisfiableTransitions -> lift_to_tuple CutUnsatisfiableTransitions.transform_program subject
  | Chaining                    -> lift_to_tuple (lift_to_program Chaining.transform_graph) subject
  | InvariantGeneration         -> lift_to_tuple InvariantGeneration.transform_program subject
  | CutZeroProbTransitions      -> lift_to_tuple CutZeroProbTransitions.transform_program subject

type outer_t = t
module PreprocessorSet =
  Set.Make(
      struct
        type t = outer_t
        let compare = compare
      end
    )

let all =
  [Chaining; CutUnreachableLocations; CutUnsatisfiableTransitions; CutZeroProbTransitions; InvariantGeneration]


type strategy = t list -> subject -> subject

let process strategy preprocessors subject =
  let execute () =
    strategy preprocessors subject
  in
  Logger.(with_log logger INFO
            (fun () -> "running_preprocessors", ["preprocessors", Util.enum_to_string show (List.enum preprocessors)])
            execute)

let process_only_once preprocessors =
  PreprocessorSet.fold (fun preprocessor subject -> MaybeChanged.unpack (transform subject preprocessor)) (PreprocessorSet.of_list preprocessors)

let rec process_til_fixpoint_ ?(wanted=PreprocessorSet.of_list all) (todos: PreprocessorSet.t) (subject: subject) : subject =
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

