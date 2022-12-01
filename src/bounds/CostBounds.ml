(** Implementation of cost-bounds.*)
open Batteries
open BoundsInst

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Approximation = Approximation.Make(PM)

  (** Returns true iff bound is not finite. *)
  let unbounded appr transition =
    Bound.is_infinity (Approximation.costbound appr transition)

  (** Infers cost-bounds from time-bounds by multiplying the costs of a transition with the time-bound of the same transition. *)
  let infer_from_timebounds program appr =
    let add_costbound transition appr =
      if unbounded appr transition then
        if Polynomials.Polynomial.is_const (Transition.cost transition) then
          Approximation.add_costbound Bound.(of_poly (Transition.cost transition) * Approximation.timebound appr transition) transition appr
        else
          let overappr_cost =
            (* Precompute and Cache *)
            let temp_vars = VarSet.diff (Program.vars program) (Program.input_vars program) in

            (* We can not look at the size bounds for predecessor transitions if transition is initial *)
            if Program.is_initial program transition then
              if VarSet.exists (flip VarSet.mem temp_vars) (Polynomials.Polynomial.vars (Transition.cost transition)) then
                Bound.infinity
              else
                Bound.of_poly (Transition.cost transition)

            (* Overapproximate the cost by looking at the sizes of incoming transitions *)
            else
              let inc_trans = List.of_enum @@ Program.pre program transition in
              let inc_size v =
                List.enum inc_trans
                |> Enum.map (fun t -> Approximation.sizebound appr t v)
                |> Bound.sum
              in
              Bound.of_poly (Transition.cost transition)
              |> Bound.substitute_f inc_size
          in
          Approximation.add_costbound Bound.(overappr_cost * Approximation.timebound appr transition) transition appr
      else
        appr
    in
    TransitionGraph.fold_edges_e add_costbound (Program.graph program) appr
end
