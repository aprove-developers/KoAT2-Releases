open Batteries

(** Provides default implementations of RankingFunctions *)

module Make(P : ProgramTypes.Program) : BoundTypes.RankingFunction
       with module Program_ = P
        and module Polynomial_ = P.Constraint_.Polynomial_
