open! OurBase
open ProbabilisticProgramModules
open Bounds

let compute_ue_diff label v =
  let ue = Option.value_exn (TransitionLabel.update label v) in
  let ue_diff_syntactic = UpdateElement.sub ue (UpdateElement.of_var v) in

  if UpdateElement.(ue_diff_syntactic =~= UpdateElement.zero) then
    UpdateElement.zero
  else
    (* Before committing to ue_diff_syntactic try to check if due to an application of the transition *)
    (* the absolute value of v decreases instead of increases *)
    let s = SMT.IncrementalZ3Solver.create () in
    let v' = Var.fresh_id Var.Int () in
    let constr = Constraints.Constraint.mk_and (UpdateElement.as_guard ue v') (TransitionLabel.guard label) in
    SMT.IncrementalZ3Solver.add s (Formulas.Formula.mk constr);
    (* search contra *)
    SMT.IncrementalZ3Solver.add_bound_comparison s `LT (Bound.of_var v) (Bound.of_var v');
    if SMT.IncrementalZ3Solver.unsatisfiable s then
      UpdateElement.zero
    else
      ue_diff_syntactic


let compute_elcb program_vars ((gt, l), v) =
  Set.to_sequence (GeneralTransition.transitions_to_target l gt)
  |> Sequence.map ~f:(fun t ->
         let label = Transition.label t in
         let ue_diff = compute_ue_diff label v in

         let ue_exp_abs_diff_bound =
           let temp_var_bound tv =
             let guard = Formulas.Formula.mk (TransitionLabel.guard label) in
             LocalSizeBound.find_bound program_vars tv guard (LocalSizeBound.c_range guard)
             |> Option.map ~f:(RationalBound.of_intbound % LocalSizeBound.as_bound % Tuple2.first)
             |> Option.value ~default:RationalBound.infinity
           in
           UpdateElement.exp_value_abs_bound ue_diff
           |> RationalBound.substitute_f (fun v ->
                  if Set.mem program_vars v then
                    RationalBound.of_var v
                  else
                    temp_var_bound v)
         in
         let ue_exp_abs_diff_poly = RationalBound.to_poly ue_exp_abs_diff_bound in
         match ue_exp_abs_diff_poly with
         | Some ue_exp_abs_diff_poly ->
             Polynomials.RationalLaurentPolynomial.(ue_exp_abs_diff_poly * TransitionLabel.probability label)
             |> RationalBound.of_overapprox_laurentpoly
         | None ->
             RationalBound.(
               of_overapprox_laurentpoly (TransitionLabel.probability label) * ue_exp_abs_diff_bound))
  |> RationalBound.sum
