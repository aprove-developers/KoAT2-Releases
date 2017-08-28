open Batteries

(** Provides all necessary types for transition systems with basic string represented locations and basic transitions *)
   
module StdLocation = TransitionGraph.StdLocation

module StdTransition = TransitionGraph.MakeTransition(ConstraintImpl.StdConstraint)

module StdTransitionGraph = TransitionGraph.MakeTransitionGraph(StdTransition)
