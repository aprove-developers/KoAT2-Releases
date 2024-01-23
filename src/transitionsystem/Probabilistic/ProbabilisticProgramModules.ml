open! OurBase
module UpdateElement = UpdateElement_
module TransitionLabel = ProbabilisticPrograms.ProbabilisticTransitionLabel
module Transition = ProbabilisticPrograms.ProbabilisticTransition
module TransitionSet = Transition_.TransitionSetOver (Transition)
module TransitionGraph = ProbabilisticPrograms.ProbabilisticTransitionGraph
module Program = ProbabilisticPrograms.ProbabilisticProgram
module RV = ProbabilisticPrograms.ProbabilisticRV
module GeneralTransition = ProbabilisticPrograms.GeneralTransition
module GeneralTransitionSet = ProbabilisticPrograms.GeneralTransitionSet
module GRV = ProbabilisticPrograms.GRV

type program_modules_t =
  (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t)
  ProgramTypes.program_modules_meta

module NonProbOverappr = struct
  module Program = ProbabilisticPrograms.ProbabilisticProgramNonProbOverappr
  module TransitionGraph = ProbabilisticPrograms.ProbabilisticTransitionGraphNonProbOverappr
  module Location = Location
  module UpdateElement = Polynomials.Polynomial

  module TransitionSet =
    Transition_.TransitionSetOver (ProbabilisticPrograms.ProbabilisticTransitionNonProbOverappr)

  module Transition = ProbabilisticPrograms.ProbabilisticTransitionNonProbOverappr
  module TransitionLabel = ProbabilisticPrograms.ProbabilisticTransitionLabelNonProbOverappr
  module RV = ProbabilisticPrograms.ProbabilisticRVNonProbOverappr

  type program_modules_t =
    (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t)
    ProgramTypes.program_modules_meta
end

(** Uses already existing overapproximation *)
module ProbabilisticAdapter :
  GenericProgram_.Adapter
    with type update_element = UpdateElement.t
     and type transition = Transition.t
     and type transition_graph = TransitionGraph.t
     and type program = Program.t = struct
  type update_element = UpdateElement.t
  type approx = Polynomials.Polynomial.t * Guard.t
  type transition = Transition.t
  type transition_graph = TransitionGraph.t
  type program = Program.t
  type grouped_transition = GeneralTransition.t
  type grouped_transition_cmp_wit = GeneralTransition.comparator_witness

  module P = Polynomials.Polynomial

  let overapprox_indeterminates ue =
    let new_var = Var.fresh_id Var.Int () in
    (P.of_var new_var, UpdateElement_.as_guard ue new_var)


  let outgoing_grouped_transitions trans_graph location =
    TransitionGraph.outgoing_gts trans_graph location |> Set.to_sequence


  let empty_grouped_transition_set = GeneralTransitionSet.empty
  let guard_of_grouped_transition = GeneralTransition.guard
  let all_grouped_transitions_of_graph = TransitionGraph.gts
  let grouped_transition_of_transition = Transition.gt

  let copy_and_modify_grouped_transition ~new_start ~add_invariant ~redirect gt =
    GeneralTransition.mk_from_labels_without_backlink ~start:new_start
      ~guard_without_invariant:(GeneralTransition.guard_without_inv gt)
      ~invariant:(Guard.mk_and (GeneralTransition.invariant gt) add_invariant)
      ~cost:(GeneralTransition.cost gt)
      ~rhss:
        (GeneralTransition.transitions gt |> Set.to_sequence
        |> Sequence.map ~f:(fun ((_, label, _) as transition) ->
               let target_location = redirect transition in
               (TransitionLabel.without_backlink label, target_location))
        |> Sequence.to_list)


  let create_new_program = Program.from_gts
end
