(** Implementation of cost-bounds.*)
open Batteries
open BoundsInst
open ProgramTypes
   
(** Returns true iff bound is not finite. *)
let unbounded appr transition =
  Bound.is_infinity (Approximation.costbound appr transition)

(** Infers cost-bounds from time-bounds by multiplying the costs of a transition with the time-bound of the same transition. *)
let infer_from_timebounds program appr =
  let add_costbound transition appr =
    if unbounded appr transition then
      Approximation.add_costbound Bound.(of_poly (Transition.cost transition) * Approximation.timebound appr transition) transition appr
    else
      appr
  in
  TransitionGraph.fold_edges_e add_costbound (Program.graph program) appr
