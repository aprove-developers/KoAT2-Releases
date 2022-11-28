open Batteries

module Location = Location
module UpdateElement = UpdateElement_
module LocationSet = Set.Make(Location)
module TransitionLabel = ProbabilisticPrograms.ProbabilisticTransitionLabel
module Transition = ProbabilisticPrograms.ProbabilisticTransition
module TransitionSet = Transition_.TransitionSetOver(Transition)(Location)
module TransitionGraph = ProbabilisticPrograms.ProbabilisticTransitionGraph
module Program = ProbabilisticPrograms.ProbabilisticProgram

module GeneralTransition = ProbabilisticPrograms.GeneralTransition
module GeneralTransitionSet = ProbabilisticPrograms.GeneralTransitionSet

module NonProbOverappr = struct
  module Program = ProbabilisticPrograms.ProbabilisticProgramNonProbOverappr
  module TransitionGraph = ProbabilisticPrograms.ProbabilisticTransitionGraphNonProbOverappr

  module LocationSet = Set.Make(Location)
  module Location = Location

  module UpdateElement = Polynomials.Polynomial

  module TransitionSet =
    Transition_.TransitionSetOver(ProbabilisticPrograms.ProbabilisticTransitionNonProbOverappr)(Location)
  module Transition = ProbabilisticPrograms.ProbabilisticTransitionNonProbOverappr
  module TransitionLabel = ProbabilisticPrograms.ProbabilisticTransitionLabelNonProbOverappr
end
