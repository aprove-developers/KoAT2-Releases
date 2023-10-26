open Formulas
open Polynomials
open ProgramTypes

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  module Loop : module type of Loop.Make (Bound) (PM)
  module Approximation : module type of Approximation.MakeForClassicalAnalysis (Bound) (PM)
  open PM

  type twn_loop = Transition.t list * (Transition.t * Loop.t) list

  val find_all_loops :
    ProofOutput.LocalProofOutput.t ->
    ?relevant_vars:VarSet.t option ->
    (Formula.t * Polynomial.t VarMap.t -> Transition.t -> bool) ->
    Program.t ->
    (Transition.t, 'a) Base.Set.t ->
    Transition.t ->
    twn_loop ProofOutput.LocalProofOutput.with_proof list
  (** copies the proof output for all elements of the result list *)

  val find_loop :
    ProofOutput.LocalProofOutput.t ->
    ?relevant_vars:VarSet.t option ->
    (Approximation.t -> Program.t -> Formula.t * Polynomial.t VarMap.t -> bool) ->
    Approximation.t ->
    Program.t ->
    TransitionSet.t ->
    Transition.t ->
    (Loop.t * (Transition.t * Polynomial.t VarMap.t) list) option
end
