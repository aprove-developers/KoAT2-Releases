(** Implemenation of a preprocessor which removes all unsatisfiable transitions. *)
open Batteries
open Formulas
open ProgramTypes
   
(** This preprocessor removes all unsatisfiable transitions from the graph. 
    Those transitions can never be part of an evaluation.
    Note that it only removes the specific transitions. 
    After the transformation the graph might contain unreachable locations, and even locations that are not connected to any transition. *)

(** Logger Preprocessor *)
let logger = Logging.(get Preprocessor)
   
(** Returns a set of transistions which have a conflicting guard (e.g. 0 > 0). *)
let unsatisfiable_transitions graph : TransitionSet.t =
  let combine (l,t,l') set =
    if SMT.Z3Solver.unsatisfiable_int (Formula.mk (TransitionLabel.guard t)) then
      TransitionSet.add (l,t,l') set
    else set in
  TransitionGraph.fold_edges_e combine graph TransitionSet.empty

(** Returns program without unsatisfiable transitions. *)
let transform_program program =
  let unsatisfiable_transitions = unsatisfiable_transitions (Program.graph program) in
  if TransitionSet.is_empty unsatisfiable_transitions then
    MaybeChanged.same program
  else
    let remove transition program =
      Logger.(log logger INFO (fun () -> "cut_unsatisfiable_transitions", ["transition", Transition.to_id_string transition]));
      Program.remove_transition program transition
    in
    MaybeChanged.changed (TransitionSet.fold remove unsatisfiable_transitions program)
