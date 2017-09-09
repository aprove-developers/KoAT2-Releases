open Batteries

(** Provides all necessary types for transition systems with basic string represented locations and basic transitions *)
   
module StdLocation = TransitionGraph.StdLocation

module StdProgram = TransitionGraph.MakeProgram(ConstraintImpl.StdConstraint)
