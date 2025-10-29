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
module UpdateElementNonRec = Polynomials.Polynomial

module TransitionLabelNonRec =
  ProbabilisticPrograms.ProbabilisticTransitionLabelNonProbOverappr.TransitionLabelNonRec

type program_modules_t =
  (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t)
  ProgramTypes.program_modules_meta

module NonProbOverappr = struct
  module Program = ProbabilisticPrograms.ProbabilisticProgramNonProbOverappr
  module TransitionGraph = ProbabilisticPrograms.ProbabilisticTransitionGraphNonProbOverappr
  module Location = Location
  module UpdateElement = PolyFunctionCall
  module UpdateElementNonRec = Polynomials.Polynomial

  module TransitionLabelNonRec =
    ProbabilisticPrograms.ProbabilisticTransitionLabelNonProbOverappr.TransitionLabelNonRec

  module TransitionSet =
    Transition_.TransitionSetOver (ProbabilisticPrograms.ProbabilisticTransitionNonProbOverappr)

  module Transition = ProbabilisticPrograms.ProbabilisticTransitionNonProbOverappr
  module TransitionLabel = ProbabilisticPrograms.ProbabilisticTransitionLabelNonProbOverappr
  module RV = ProbabilisticPrograms.ProbabilisticRVNonProbOverappr

  type program_modules_t =
    (TransitionLabel.t * TransitionLabel.comparator_witness * TransitionGraph.t)
    ProgramTypes.program_modules_meta
end
