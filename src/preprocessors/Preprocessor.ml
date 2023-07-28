open OurBase
open ProgramModules

let logger = Logging.(get Preprocessor)

(* The ordering of contructors below influences the order of the resulting Set.t *)
type _ t =
  | CutZeroProbTransitions: ProbabilisticPrograms.ProbabilisticProgram.t t
  | CutUnreachableLocations: 'p t
  | CutUnsatisfiableTransitions: 'p t
  | EliminateNonContributors: 'p t
  | EliminateTempVars: Program.t t
  | Chaining: Program.t t
  | ChainingConservative: Program.t t
  | InvariantGeneration: 'p t

let show: type p. p t -> string = function
  | CutZeroProbTransitions -> "zeroprobtransitions"
  | CutUnreachableLocations -> "reachable"
  | CutUnsatisfiableTransitions -> "sat"
  | Chaining -> "chaining"
  | ChainingConservative -> "chaining-conservative"
  | EliminateNonContributors -> "eliminate"
  | EliminateTempVars -> "tmp"
  | InvariantGeneration -> "invgen"

let affects: type p. p t -> p t list = function
  | CutZeroProbTransitions -> [ ]
  | CutUnreachableLocations -> [EliminateNonContributors]
  | InvariantGeneration -> [ CutUnsatisfiableTransitions ]
  | CutUnsatisfiableTransitions -> [ CutUnreachableLocations; EliminateNonContributors]
  | EliminateNonContributors -> []
  | EliminateTempVars -> [EliminateNonContributors; CutUnsatisfiableTransitions]
  | Chaining -> [CutUnsatisfiableTransitions; Chaining; ChainingConservative; InvariantGeneration]
  | ChainingConservative -> [CutUnsatisfiableTransitions; Chaining; ChainingConservative; InvariantGeneration]


(* Chaining might introduce MANY different temporary variables. To mitigate this we normalise their names*)
let normalise_temp_vars program =
  let temp_vars = OurBase.Sequence.memoize @@ OurBase.Sequence.uniter (Var.fresh_id Var.Int) in
  Program.map_graph (fun graph ->
    let trans = TransitionGraph.transitions graph in
    Base.Set.fold
      ~f:(fun graph (l,t,l') -> TransitionGraph.replace_edge_e (l,t,l') (l,TransitionLabel.rename_temp_vars t temp_vars,l') graph)
      trans ~init:graph
  ) program


let lift_to_program (transform: TransitionGraph.t -> TransitionGraph.t MaybeChanged.t) (program: Program.t) =
  MaybeChanged.(transform (Program.graph program) >>= fun graph -> same (Program.map_graph (fun _ -> graph) program))

let transform (type p) (module P: ProgramTypes.ClassicalProgramModules with type Program.t = p)
    (subject: P.Program.t) (p: P.Program.t t): P.Program.t MaybeChanged.t =
  match p with
  | CutUnreachableLocations ->
    let module CUL = CutUnreachableLocations.Make(P) in
    CUL.transform_program subject
  | CutUnsatisfiableTransitions ->
    let module CUT = CutUnsatisfiableTransitions.Make(P) in CUT.transform_program subject
  | CutZeroProbTransitions -> CutZeroProbTransitions.transform_program subject
  | Chaining -> (MaybeChanged.map normalise_temp_vars  % lift_to_program Chaining.transform_graph) subject
  | ChainingConservative -> (MaybeChanged.map normalise_temp_vars  % lift_to_program (Chaining.transform_graph ~conservative:true)) subject
  | EliminateNonContributors ->
    let module ENC = EliminateNonContributors.Make(P) in ENC.eliminate subject
  | EliminateTempVars -> EliminateTempVars.eliminate_tmp_vars subject
  | InvariantGeneration ->
    let module IG = InvariantGeneration.Make(P) in IG.transform_program subject

let all_classical: Program.t t list =
  [Chaining; ChainingConservative; CutUnreachableLocations; CutUnsatisfiableTransitions; EliminateNonContributors; InvariantGeneration; EliminateTempVars]

let all_probabilistic: ProbabilisticPrograms.ProbabilisticProgram.t t list =
  [CutZeroProbTransitions; CutUnreachableLocations; CutUnsatisfiableTransitions; EliminateNonContributors; InvariantGeneration]

let all_generic: 'a. 'a t list =
  [CutUnreachableLocations; CutUnsatisfiableTransitions; EliminateNonContributors; InvariantGeneration]

type 'p strategy = (module ProgramTypes.ClassicalProgramModules with type Program.t = 'p) -> 'p t list -> 'p -> 'p

let process (type p) (module P: ProgramTypes.ClassicalProgramModules with type Program.t = p)
    (strat: P.Program.t strategy) preprocessors subject =
  let execute () =
    strat (module P) preprocessors subject
  in
  Logger.(with_log logger INFO
            (fun () -> "running_preprocessors", ["preprocessors", Util.sequence_to_string ~f:show (Sequence.of_list preprocessors)])
              execute)

let process_only_once: type p. p strategy = fun (module P) preprocessors p ->
  Set.fold ~f:(fun subject preprocessor -> MaybeChanged.unpack (transform (module P) subject preprocessor)) (Set.Poly.of_list preprocessors) ~init:p

let process_till_fixpoint_ (type p) (module P: ProgramTypes.ClassicalProgramModules with type Program.t = p)
    ~(wanted:P.Program.t t Set.Poly.t) =
  let rec iterate (todos: P.Program.t t Set.Poly.t) (subject: P.Program.t): P.Program.t =
    if Set.is_empty todos then subject
    else
      let (preprocessor, others) =
        let m = Set.Poly.min_elt_exn todos in
        (m, Set.Poly.remove todos m)
      in
      let maybe_changed = transform (module P) subject preprocessor in
      let new_preprocessor_set =
        if MaybeChanged.has_changed maybe_changed then
          Set.Poly.(preprocessor |> affects |> of_list |> inter wanted |> union others)
        else others in
      iterate new_preprocessor_set (MaybeChanged.unpack maybe_changed)
  in
  iterate

let process_till_fixpoint: type p. p strategy = fun (module P) preprocessors subject ->
  let set = OurBase.Set.Poly.of_list preprocessors in
  process_till_fixpoint_ (module P) ~wanted:set set subject

let all_strategies: 'p. 'p strategy list = [process_only_once; process_till_fixpoint]
