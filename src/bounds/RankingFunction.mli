open Batteries

(** Provides default implementations of RankingFunctions *)

module Make(P : ProgramTypes.Program) : BoundTypes.RankingFunction
       with module Program_ = P
        and module Constraints_ = P.Constraint_
        and module Polynomial_ = P.Constraint_.Polynomial_
        and module ParameterPolynomial_ = Polynomials.Make(P.Constraint_.Polynomial_)
        and module ParameterFormula_= Formula.Make(Polynomials.Make(P.Constraint_.Polynomial_))
