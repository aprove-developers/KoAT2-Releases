open Batteries
open BoundsInst
open ProgramTypes

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

(** This function is similar to TrivialTimeBounds, and derives time bounds of 1 for general transitions.
    However when considering general transitions is has to be ensured that the start location can not be reached from any target location,
    i.e. no target location is in the same scc as the start location*)
let compute_generaltransitions program appr =
  let graph = Program.graph program in
  let (_, scc_number) = SCC.scc graph in
  let same_scc l1 l2 =
    scc_number l1 = scc_number l2
  in
  let one_bounded_gts =
    TransitionGraph.generalized_transitions graph
    |> GeneralTransitionSet.filter (fun gt -> GeneralTransition.targets gt |> LocationSet.for_all (not % same_scc (GeneralTransition.start gt)))
  in

  GeneralTransitionSet.fold (fun gt appr -> Approximation.add_timebound_gt Bound.one gt (Approximation.add_exptimebound RealBound.one gt appr)) one_bounded_gts appr
  (* add corresponding costbounds *)
  |> GeneralTransitionSet.fold
      (
        fun gt ->
          Approximation.add_expcostbound
            (RealBound.appr_substition_abs_all (BoundsHelper.nonprob_incoming_size program appr gt) (GeneralTransition.cost gt))
            gt
      )
      one_bounded_gts
