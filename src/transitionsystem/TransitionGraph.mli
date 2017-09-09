open Batteries

(** Provides default modules to create locations, transitions and transitionsystems *)

module StdLocation : TransitionGraphTypes.Location

module MakeProgram(C : ConstraintTypes.Constraint) : TransitionGraphTypes.Program
       with module Constraint_ = C
