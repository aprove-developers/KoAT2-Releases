open Batteries

let logger = Logging.(get Preprocessor)

type subject = Program.t * Approximation.t

type t =
  | CutUnreachableLocations
  | CutUnsatisfiableTransitions
  | EliminateNonContributors
  | Chaining
  | InvariantGeneration[@@deriving ord, eq]

let show = function
  | CutUnreachableLocations -> "reachable"
  | CutUnsatisfiableTransitions -> "sat"
  | Chaining -> "chaining"
  | EliminateNonContributors -> "eliminate"
  | InvariantGeneration -> "invgen"

let affects = function
  | CutUnreachableLocations -> [EliminateNonContributors]
  | InvariantGeneration -> [CutUnsatisfiableTransitions]
  | CutUnsatisfiableTransitions -> [CutUnreachableLocations; Chaining; EliminateNonContributors]
  | EliminateNonContributors -> []
  | Chaining -> [CutUnsatisfiableTransitions; Chaining; InvariantGeneration]

let lift_to_program transform program =
  MaybeChanged.(transform (Program.graph program) >>= (fun graph -> same (Program.map_graph (fun _ -> graph) program)))

let lift_to_tuple transform tuple =
  MaybeChanged.(transform (Tuple2.first tuple) >>= (fun program -> same (Tuple2.map1 (fun _ -> program) tuple)))

let transform subject = function
  | CutUnreachableLocations -> lift_to_tuple CutUnreachableLocations.transform_program subject
  | CutUnsatisfiableTransitions -> lift_to_tuple CutUnsatisfiableTransitions.transform_program subject
  | Chaining -> lift_to_tuple (lift_to_program Chaining.transform_graph) subject
  | EliminateNonContributors -> lift_to_tuple EliminateNonContributors.eliminate subject
  | InvariantGeneration -> lift_to_tuple InvariantGeneration.transform_program subject

type outer_t = t
module PreprocessorSet =
  Set.Make(
      struct
        type t = outer_t
        let compare = compare
      end
    )

let all =
  [Chaining; CutUnreachableLocations; CutUnsatisfiableTransitions; EliminateNonContributors; InvariantGeneration]


type strategy = t list -> subject -> subject

let normalise_temp_vars (program, appr) =
  let temp_vars = LazyList.from (Var.fresh_id Var.Int) in
  let normalised =
    Program.map_graph (fun graph ->
      let trans = ProgramTypes.TransitionGraph.transitions graph in
      ProgramTypes.TransitionSet.fold
        (fun (l,t,l') -> ProgramTypes.TransitionGraph.replace_edge_e (l,t,l') (l,TransitionLabel.rename_temp_vars t temp_vars,l'))
        trans graph
    ) program
  in
  normalised, appr

let process strategy preprocessors subject =
  let execute () =
    strategy preprocessors subject
    |> normalise_temp_vars
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
    let (preprocessor, others) = PreprocessorSet.pop_min todos in
    let maybe_changed = transform subject preprocessor in
    let new_preprocessor_set =
      if MaybeChanged.has_changed maybe_changed then
        PreprocessorSet.(preprocessor |> affects |> of_list |> inter wanted |> union others)
      else others in
    process_til_fixpoint_ ~wanted new_preprocessor_set (MaybeChanged.unpack maybe_changed)

let process_til_fixpoint preprocessors subject =
  let set = PreprocessorSet.of_list preprocessors in
  process_til_fixpoint_ ~wanted:set set subject

let all_strategies = [process_only_once; process_til_fixpoint]

