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
