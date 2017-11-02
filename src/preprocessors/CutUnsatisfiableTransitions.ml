open Batteries
open Formulas
open Program.Types
   
(** This preprocessor removes all unsatisfiable transitions from the graph. 
    Those transitions can never be part of an evaluation.
    Note that it only removes the specific transitions. 
    After the transformation the graph might contain unreachable locations, and even locations that are not connected to any transition. *)

module TransitionSet = Set.Make(Transition)

let unsatisfiable_transitions graph : TransitionSet.t =
  let combine (l,t,l') set =
    if SMT.Z3Solver.unsatisfiable (Formula.mk (TransitionLabel.guard t)) then
      TransitionSet.add (l,t,l') set
    else set in
  TransitionGraph.fold_edges_e combine graph TransitionSet.empty
  
let transform_program program =
  let unsatisfiable_transitions = unsatisfiable_transitions (Program.graph program) in
  if TransitionSet.is_empty unsatisfiable_transitions then
    MaybeChanged.same program
  else
    MaybeChanged.changed (TransitionSet.fold (flip Program.remove_transition) unsatisfiable_transitions program)
