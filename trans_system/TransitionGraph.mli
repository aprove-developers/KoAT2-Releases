open Batteries

module StdLocation : TransitionGraphTypes.Location

module MakeTransition(C : ConstraintTypes.Constraint) : TransitionGraphTypes.Transition
       with module Constraint_ = C

module MakeTransitionGraph(T : TransitionGraphTypes.Transition) : TransitionGraphTypes.Graph
     with module Transition_ = T
     and module Location_ = StdLocation
