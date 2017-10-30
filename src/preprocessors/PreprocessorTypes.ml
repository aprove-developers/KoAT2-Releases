open Batteries

(** Provides all module types related to preprocessors *)

type subject = Program.t * Approximation.t

type preprocessor =
  | CutUnreachable
  | TrivialTimeBounds
  | CutUnsatisfiableTransitions
  | Chaining [@@deriving show, ord]

(** Returns all the preprocessors that might successfully run after a run of the specific preprocessor. *)
let affects = function
  | CutUnreachable -> []
  | TrivialTimeBounds -> []
  | CutUnsatisfiableTransitions -> [CutUnreachable; Chaining]
  | Chaining -> [CutUnsatisfiableTransitions]

(** This preprocessor cuts all unreachable locations (and all transitions connected to them) from the program. *)
module CutUnreachable =
  struct
    module LocationSet = Set.Make(Program.Location)

    (** Returns a set of all locations which are reachable from the given start location. *)
    let reachable_locations graph start : LocationSet.t  =
      let module Traverse = Graph.Traverse.Bfs(Program.TransitionGraph) in
      Traverse.fold_component LocationSet.add LocationSet.empty graph start

    let unreachable_locations graph start : LocationSet.t =
      LocationSet.diff (Program.TransitionGraph.locations graph) (reachable_locations graph start)

    let transform_program program = 
      let unreachable_locations = unreachable_locations (Program.graph program) (Program.start program) in
      if LocationSet.is_empty unreachable_locations then
        MaybeChanged.same program
      else
        MaybeChanged.changed (LocationSet.fold (flip Program.remove_location) unreachable_locations program)

  end

(** This preprocessor infers for all transitions which are not part of an scc a time bound of their cost.
    Those transitions can only be executed once and preprocessing might increase performance and also might lead to better bounds. *)
module TrivialTimeBounds =
  struct
    module SCC = Graph.Components.Make(Program.TransitionGraph)

    let transform (program, appr) =
      let graph = Program.graph program in
      let (_, scc_number) = SCC.scc graph in
      let same_scc l1 l2 =
        scc_number l1 = scc_number l2 in
      let one_bounded_transitions =
        Program.TransitionGraph.transitions graph
        |> Program.TransitionSet.filter (fun (l,t,l') -> not (same_scc l l')) in
      if Program.TransitionSet.is_empty one_bounded_transitions then
        MaybeChanged.same (program, appr)
      else
        MaybeChanged.changed (program, (Program.TransitionSet.fold (fun (l,t,l') appr -> Approximation.add_timebound Bound.one (Program.TransitionGraph.find_edge graph l l') appr) one_bounded_transitions appr))
      
  end

(** This preprocessor removes all unsatisfiable transitions from the graph. 
    Those transitions can never be part of an evaluation.
    Note that it only removes the specific transitions. 
    After the transformation the graph might contain unreachable locations, and even locations that are not connected to any transition. *)
module CutUnsatisfiableTransitions =
  struct
    module TransitionSet = Set.Make(Program.Transition)
    open Formulas

    let unsatisfiable_transitions graph : TransitionSet.t =
      let combine (l,t,l') set =
        if SMT.Z3Solver.unsatisfiable (Formula.mk (TransitionLabel.guard t)) then
          TransitionSet.add (l,t,l') set
        else set in
      Program.TransitionGraph.fold_edges_e combine graph TransitionSet.empty
        
    let transform_program program =
      let unsatisfiable_transitions = unsatisfiable_transitions (Program.graph program) in
      if TransitionSet.is_empty unsatisfiable_transitions then
        MaybeChanged.same program
      else
        MaybeChanged.changed (TransitionSet.fold (flip Program.remove_transition) unsatisfiable_transitions program)
      
  end

module Chaining =
  struct

    (** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location,
        making the location obsolete. *)
    let skip_location location graph =
      Program.TransitionGraph.(pred_e, succ_e)
      |> Tuple2.mapn (fun f -> f graph location)
      |> Tuple2.mapn List.enum
      |> uncurry Enum.cartesian_product
      |> Enum.map (fun ((l,t,_), (_,t',l')) -> (l, TransitionLabel.append t t', l'))
      |> Program.add_edges graph      

    (** Returns if the specific location is chainable in the graph. *)
    let chainable graph location : bool =
      let open Program.TransitionGraph in
      not (mem_edge graph location location)
      && out_degree graph location >= 1
      && in_degree graph location >= 1

    (** Performs a chaining step removing the location from the graph. *)
    let chain location graph : Program.TransitionGraph.t =
      Program.TransitionGraph.remove_vertex (skip_location location graph) location

    let transform_graph (graph: Program.TransitionGraph.t): Program.TransitionGraph.t MaybeChanged.t =
      let try_chaining location maybe_changed_graph =
        let open MaybeChanged in
        maybe_changed_graph >>= (fun graph ->
          if chainable graph location then
            changed (chain location graph)
          else
            same graph)
      in Program.TransitionGraph.fold_vertex try_chaining graph (MaybeChanged.same graph)

  end

(** Transforms a preprocessing step with the specific preprocessor on the subject.
    Results in a subject that might be changed. *)
let transform (subject: subject) (preprocessor: preprocessor): subject MaybeChanged.t =
  match preprocessor with
  | CutUnreachable -> MaybeChanged.lift_to_subject CutUnreachable.transform_program subject
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
  PreprocessorSet.of_list [CutUnreachable; TrivialTimeBounds; CutUnsatisfiableTransitions; Chaining]
  
(** Applies each preprocessor exactly one time on the subject. *)
let process_only_once preprocessors =
  PreprocessorSet.fold (fun preprocessor subject -> MaybeChanged.unpack (transform subject preprocessor)) (PreprocessorSet.of_list preprocessors)

let rec process_til_fixpoint_ ?(wanted=all_preprocessors) (todos: PreprocessorSet.t) (subject: subject) : subject =
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

(** Applies the preprocessors continously until a fixpoint is reached, such that no preprocessor is able to do another successful preprocessing step. *)
let process_til_fixpoint preprocessors =
  let set = PreprocessorSet.of_list preprocessors in
  process_til_fixpoint_ ~wanted:set set
  
