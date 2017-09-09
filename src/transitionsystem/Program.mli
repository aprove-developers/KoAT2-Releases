open Batteries

(** Provides default modules to create locations, transitions and transitionsystems *)

module Make(C : ConstraintTypes.Constraint) : ProgramTypes.Program
       with module Constraint_ = C
