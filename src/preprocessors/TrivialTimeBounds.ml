open Batteries
open Program.Types
   
(** This preprocessor infers for all transitions which are not part of an scc a time bound of their cost.
    Those transitions can only be executed once and preprocessing might increase performance and also might lead to better bounds. *)

module SCC = Graph.Components.Make(TransitionGraph)

let transform (program, appr) =
  let graph = Program.graph program in
  let (_, scc_number) = SCC.scc graph in
  let same_scc l1 l2 =
    scc_number l1 = scc_number l2 in
  let one_bounded_transitions =
    TransitionGraph.transitions graph
    |> TransitionSet.filter (fun (l,t,l') -> not (same_scc l l')) in
  if TransitionSet.is_empty one_bounded_transitions then
    MaybeChanged.same (program, appr)
  else
    appr
    |> TransitionSet.fold (fun (l,t,l') appr -> Approximation.add_timebound ((Bound.of_poly % TransitionLabel.cost) t) (TransitionGraph.find_edge graph l l') appr) one_bounded_transitions
    |> fun appr -> MaybeChanged.changed (program, appr)
