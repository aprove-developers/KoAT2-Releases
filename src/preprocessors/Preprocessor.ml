open! OurBase
open ProgramModules

let logger = Logging.(get Preprocessor)

(* The ordering of contructors below influences the order of the resulting Set.t *)
type _ t =
  | CutZeroProbTransitions : ProbabilisticPrograms.ProbabilisticProgram.t t
  | CutUnreachableLocations : 'p t
  | CutUnsatisfiableTransitions : 'p t
  | EliminateNonContributors : 'p t
  | EliminateTempVars : Program.t t
  | Chaining : Program.t t
  | ChainingConservative : Program.t t
  | InvariantGeneration : 'p t

let show : type p. p t -> string = function
  | CutZeroProbTransitions -> "zeroprobtransitions"
  | CutUnreachableLocations -> "reachable"
  | CutUnsatisfiableTransitions -> "sat"
  | Chaining -> "chaining"
  | ChainingConservative -> "chaining-conservative"
  | EliminateNonContributors -> "eliminate"
  | EliminateTempVars -> "tmp"
  | InvariantGeneration -> "invgen"


let affects : type p. p t -> p t list = function
  | CutZeroProbTransitions -> []
  | CutUnreachableLocations -> [ EliminateNonContributors ]
  | InvariantGeneration -> [ CutUnsatisfiableTransitions ]
  | CutUnsatisfiableTransitions -> [ CutUnreachableLocations; EliminateNonContributors ]
  | EliminateNonContributors -> []
  | EliminateTempVars -> [ EliminateTempVars; EliminateNonContributors; CutUnsatisfiableTransitions ]
  | Chaining -> [ CutUnsatisfiableTransitions; Chaining; ChainingConservative; InvariantGeneration ]
  | ChainingConservative ->
      [ CutUnsatisfiableTransitions; Chaining; ChainingConservative; InvariantGeneration ]


(* Chaining might introduce MANY different temporary variables. To mitigate this we normalise their names*)
let normalise_temp_vars program =
  let temp_vars = Sequence.memoize @@ Sequence.uniter (Var.fresh_id Var.Int) in
  Program.map_graph
    (fun graph ->
      let trans = TransitionGraph.transitions graph in
      Set.fold
        ~f:(fun graph (l, t, l') ->
          TransitionGraph.replace_edge_e (l, t, l')
            (l, TransitionLabel.rename_temp_vars t temp_vars, l')
            graph)
        trans ~init:graph)
    program


let lift_to_program (transform : TransitionGraph.t -> TransitionGraph.t MaybeChanged.t) (program : Program.t)
    =
  MaybeChanged.(
    transform (Program.graph program) >>= fun graph -> same (Program.map_graph (fun _ -> graph) program))


let all_classical : Program.t t list =
  [
    Chaining;
    ChainingConservative;
    CutUnreachableLocations;
    CutUnsatisfiableTransitions;
    EliminateNonContributors;
    InvariantGeneration;
    EliminateTempVars;
  ]


let all_probabilistic : ProbabilisticPrograms.ProbabilisticProgram.t t list =
  [
    CutZeroProbTransitions;
    CutUnreachableLocations;
    CutUnsatisfiableTransitions;
    EliminateNonContributors;
    InvariantGeneration;
  ]


let all_generic : 'a. 'a t list =
  [ CutUnreachableLocations; CutUnsatisfiableTransitions; EliminateNonContributors; InvariantGeneration ]


type strategy = {
  run_strategy : 'p. run_preprocessor:('p t -> 'p -> 'p MaybeChanged.t) -> 'p t list -> 'p -> 'p;
}

let run_strategy s = s.run_strategy

let process_only_once : strategy =
  let strat_f ~run_preprocessor preprocessors p =
    let preprocs_set = Set.Poly.of_list preprocessors in
    Set.fold ~init:p ~f:(fun prog preproc -> MaybeChanged.unpack (run_preprocessor preproc prog)) preprocs_set
  in
  { run_strategy = strat_f }


let process_till_fixpoint_ ~run_preprocessor ~(wanted : 'a t Set.Poly.t) =
  let rec iterate (todos : 'a t Set.Poly.t) (subject : 'a) : 'a =
    if Set.is_empty todos then
      subject
    else
      let preprocessor, others =
        let m = Set.Poly.min_elt_exn todos in
        (m, Set.Poly.remove todos m)
      in
      let maybe_changed = run_preprocessor preprocessor subject in
      let new_preprocessor_set =
        if MaybeChanged.has_changed maybe_changed then
          Set.Poly.(preprocessor |> affects |> of_list |> inter wanted |> union others)
        else
          others
      in
      iterate new_preprocessor_set (MaybeChanged.unpack maybe_changed)
  in
  iterate


let process_till_fixpoint : strategy =
  let strat_f ~run_preprocessor preprocessors subject =
    let set = Set.Poly.of_list preprocessors in
    process_till_fixpoint_ ~run_preprocessor ~wanted:set set subject
  in
  { run_strategy = strat_f }


let all_strategies : strategy list = [ process_only_once; process_till_fixpoint ]

module Make
    (PM : ProgramTypes.ProgramModules)
    (CPM : ProgramTypes.ClassicalProgramModules)
    (Eq : sig
      val eq : (PM.Program.t, CPM.Program.t) Type_equal.t
    end) =
struct
  open! PM

  let transform (t : PM.Program.t t) (subject : PM.Program.t) : PM.Program.t MaybeChanged.t =
    match t with
    | CutUnreachableLocations ->
        let module CUL = CutUnreachableLocations.Make (PM) in
        CUL.transform_program subject
    | CutUnsatisfiableTransitions ->
        let module CUT = CutUnsatisfiableTransitions.Make (PM) in
        CUT.transform_program subject
    | CutZeroProbTransitions -> CutZeroProbTransitions.transform_program subject
    | Chaining -> (MaybeChanged.map normalise_temp_vars % lift_to_program Chaining.transform_graph) subject
    | ChainingConservative ->
        (MaybeChanged.map normalise_temp_vars % lift_to_program (Chaining.transform_graph ~conservative:true))
          subject
    | EliminateNonContributors ->
        let module ENC = EliminateNonContributors.Make (PM) in
        ENC.eliminate logger subject
    | EliminateTempVars ->
        MaybeChanged.(EliminateTempVars.eliminate_tmp_vars subject >>= EliminateTempVars.tmp_vars_to_nondet)
    | InvariantGeneration ->
        let module IG = InvariantGeneration.Make (CPM) in
        Type_equal.(
          conv Eq.eq subject |> IG.transform_program
          |> MaybeChanged.map (Type_equal.conv (Type_equal.sym Eq.eq)))


  let process (strat : strategy) preprocessors subject =
    let execute () =
      (* strat preprocessors subject *)
      run_strategy strat ~run_preprocessor:transform preprocessors subject
    in
    Logger.(
      with_log logger INFO
        (fun () ->
          ( "running_preprocessors",
            [ ("preprocessors", Util.sequence_to_string ~f:show (Sequence.of_list preprocessors)) ] ))
        execute)
end

module MakeForClassicalProgramModules (CPM : ProgramTypes.ClassicalProgramModules) = struct
  include
    Make (CPM) (CPM)
      (struct
        let eq = Type_equal.refl
      end)
end

module StandardProgram = MakeForClassicalProgramModules (ProgramModules)

module ProbabilisticWithOverappr =
  Make (ProbabilisticProgramModules) (ProbabilisticProgramModules.NonProbOverappr)
    (struct
      let eq = ProbabilisticPrograms.Equalities.program_equalities
    end)
