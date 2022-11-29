(** Modules used to infer time-bounds for transitions which are not part of a scc. *)
open Batteries
open BoundsInst
open ProgramModules

(** This preprocessor infers for all transitions which are not part of an scc a time bound of one.
    Those transitions can only be executed once and preprocessing might increase performance and also might lead to better bounds. *)

module Make(PM: ProgramTypes.ProgramModules) = struct
  open PM

  module Approximation = Approximation.Make(PM)

  (** Transition graph represents scc. *)
  module SCC = Graph.Components.Make(TransitionGraph)

  (** This preprocessor infers for all transitions which are not part of an scc a time bound of one.
      Those transitions can only be executed once and preprocessing might increase performance and also might lead to better bounds. *)
  let compute program appr =
    let graph = Program.graph program in
    let (_, scc_number) = SCC.scc graph in
    let same_scc l1 l2 =
      scc_number l1 = scc_number l2
    in
    let one_bounded_transitions =
      TransitionGraph.transitions graph
      |> TransitionSet.filter (fun (l,t,l') -> not (same_scc l l'))
    in
    TransitionSet.fold (Approximation.add_timebound Bound.one) one_bounded_transitions appr
end

include Make(ProgramModules)
