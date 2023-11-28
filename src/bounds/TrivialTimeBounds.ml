open! OurBase
(** Modules used to infer time-bounds for transitions which are not part of a scc. *)

(** This preprocessor infers for all transitions which are not part of an scc a time bound of one.
    Those transitions can only be executed once and preprocessing might increase performance and also might lead to better bounds. *)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ProgramModules) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)

  type appr = Approximation.t

  module SCC = Graph.Components.Make (TransitionGraph)
  (** Transition graph represents scc. *)

  let all_trivial_transitions program =
    let graph = Program.graph program in
    let _, scc_number = SCC.scc graph in
    let same_scc l1 l2 = scc_number l1 = scc_number l2 in
    TransitionGraph.transitions graph |> Base.Set.filter ~f:(fun (l, t, l') -> not (same_scc l l'))


  let compute program appr =
    let one_bounded_transitions = all_trivial_transitions program in
    Base.Set.fold ~f:(flip @@ Approximation.add_timebound Bound.one) one_bounded_transitions ~init:appr
end

module Classical (Bound : BoundType.Bound) = struct
  include Make (Bound) (ProgramModules)
end

module Probabilistic = struct
  module TrivialTimeBoundsProbabilistic = Make (Bounds.Bound) (ProbabilisticProgramModules)

  let compute program (apprs : Approximation.Probabilistic.apprs) =
    let open Approximation.Probabilistic in
    let trivial_transitions = TrivialTimeBoundsProbabilistic.all_trivial_transitions program in

    let class_appr =
      Set.fold
        ~f:(fun class_appr trivial_transition ->
          ClassicalApproximation.add_timebound Bounds.Bound.one trivial_transition class_appr)
        ~init:apprs.class_appr trivial_transitions
    in

    let appr =
      let trivial_gts =
        let open ProbabilisticProgramModules in
        GeneralTransitionSet.map ~f:Transition.gt trivial_transitions
        |> Set.filter ~f:(fun gt -> Set.is_subset (GeneralTransition.transitions gt) ~of_:trivial_transitions)
      in
      Set.fold
        ~f:(fun appr trivial_gt -> ExpApproximation.add_timebound Bounds.RationalBound.one trivial_gt appr)
        ~init:apprs.appr trivial_gts
    in

    { class_appr; appr }
end

include Classical (Bounds.Bound)
