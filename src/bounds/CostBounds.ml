open! OurBase
(** Implementation of cost-bounds.*)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)

  (** Returns true iff bound is not finite. *)
  let unbounded appr transition = Bound.is_infinity (Approximation.costbound appr transition)

  (** Infers cost-bounds from time-bounds by multiplying the costs of a transition with the time-bound of the same transition. *)
  let infer_from_timebounds_for_transitions program transitions appr =
    let add_costbound transition appr =
      if unbounded appr transition then
        if Polynomials.Polynomial.is_const (Transition.cost transition) then
          Approximation.add_costbound
            Bound.(of_intpoly (Transition.cost transition) * Approximation.timebound appr transition)
            transition appr
        else
          let overappr_cost =
            (* Precompute and Cache *)
            let temp_vars = Set.diff (Program.vars program) (Program.input_vars program) in

            if Program.is_initial program transition then
              (* We can not look at the size bounds for predecessor transitions if transition is initial *)
              Bound.of_intpoly (Transition.cost transition)
              |> Bound.substitute_f (fun v ->
                     if Set.mem temp_vars v then
                       Bound.infinity
                     else
                       Bound.of_var v)
            else
              (* Overapproximate the cost by looking at the sizes of incoming transitions *)
              let inc_trans = Set.to_sequence @@ Program.pre program transition in
              let inc_size v =
                Sequence.map ~f:(fun t -> Approximation.sizebound appr t v) inc_trans |> Bound.sum
              in
              Bound.of_intpoly (Transition.cost transition) |> Bound.substitute_f inc_size
          in
          Approximation.add_costbound
            Bound.(overappr_cost * Approximation.timebound appr transition)
            transition appr
      else
        appr
    in
    Set.fold ~init:appr ~f:(fun appr t -> add_costbound t appr) transitions


  let infer_from_timebounds program appr =
    infer_from_timebounds_for_transitions program (Program.transitions program) appr
end
