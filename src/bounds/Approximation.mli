open Batteries

(** Provides default implementations of an approximation *)

module Make(P : ProgramTypes.Program) : BoundTypes.Approximation with   
         module Program_ = P
