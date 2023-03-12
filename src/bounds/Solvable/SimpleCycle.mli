open Automorphism
open Batteries
open Formulas
open Polynomials
open ProgramTypes

module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  module Loop: module type of Loop.Make(PM)
  module Transformation: module type of Transformation.Make(PM)
  module Approximation: module type of Approximation.MakeForClassicalAnalysis(PM)

  open PM

  val find_loops :
    ?relevant_vars: VarSet.t option ->
    ?transformation_type: [< `NoTransformation | `TWNTransform > `NoTransformation] ->
    (Approximation.t -> Transition.t -> Program.t ->  Formula.t * Polynomial.t var_map -> bool) ->
    Approximation.t ->
    Program.t ->
    TransitionSet.t ->
    Transition.t ->
    (Transition.t list * (Transition.t * (Loop.t * Automorphism.t option)) list) option

  val find_loop :
    ?relevant_vars: VarSet.t option ->
    (Approximation.t -> Program.t ->  Formula.t * Polynomial.t var_map -> bool) ->
    Approximation.t ->
    Program.t ->
    TransitionSet.t ->
    Transition.t ->
    (Loop.t * (Transition.t * Polynomial.t var_map) list) option
end
