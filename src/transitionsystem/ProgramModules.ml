open! OurBase

(** Modules relevant for working with programs *)
module ProgramModulesOver : ProgramTypes.ClassicalProgramModules = struct
  module UpdateElement = Polynomials.Polynomial
  module TransitionLabel = TransitionLabel_
  module Transition = Transition_.MakeClassical (TransitionLabel)
  module TransitionSet = Transition_.TransitionSetOver (Transition)
  module TransitionGraph = TransitionGraph_.TransitionGraph
  module Program = Program_.ClassicalProgram
  module RV = RVGTypes.MakeRV (TransitionLabel) (Transition)

  type program_modules_t =
    (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t)
    ProgramTypes.program_modules_meta
end

(* here we can not simply use include ProgramModulesOver(Location) since we rely on the specialized versions *)
module Program = Program_
module UpdateElement = Polynomials.Polynomial
module TransitionGraph = TransitionGraph_
module TransitionLabel = TransitionLabel_
module Transition = Transition_
module TransitionSet = Transition_.TransitionSetOver (Transition)
module RV = RVGTypes.MakeRV (TransitionLabel) (Transition)

type program_modules_t =
  (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t)
  ProgramTypes.program_modules_meta

(** Trivial implementation of overapproxmation in classical programs *)
module ClassicAdapter :
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
  type grouped_transition = Transition.t
  type grouped_transition_cmp_wit = Transition.comparator_witness

  (** Overapproximating of normal polynomials is not required and the polynomial is returned as is *)
  let overapprox_indeterminates poly = (poly, Guard.mk_true)

  let outgoing_grouped_transitions trans_graph location =
    TransitionGraph.succ_e trans_graph location |> Sequence.of_list


  let empty_grouped_transition_set = TransitionSet.empty
  let guard_of_grouped_transition = TransitionLabel.guard % Transition.label
  let all_grouped_transitions_of_graph = TransitionGraph.transitions
  let grouped_transition_of_transition = identity

  let copy_and_modify_grouped_transition ~new_start ~add_invariant ~redirect ((l, label, l') as transition) =
    let new_label = TransitionLabel.fresh_id label |> flip TransitionLabel.add_invariant add_invariant in
    (new_start, new_label, redirect transition)


  let create_new_program location tset = Program.from_sequence location (Set.to_sequence tset)
end
