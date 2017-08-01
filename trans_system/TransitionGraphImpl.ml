open Batteries

module StdLocation = TransitionGraph.StdLocation

module StdTransition = TransitionGraph.MakeTransition(ConstraintImpl.StdConstraint)

module StdTransitionGraph = TransitionGraph.MakeTransitionGraph(StdTransition)
