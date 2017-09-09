open Batteries

(** Provides all necessary types for transition systems with basic string represented locations and basic transitions *)
   
module StdProgram = Program.Make(ConstraintImpl.StdConstraint)
