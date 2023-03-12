open Batteries
open ProbabilisticProgramModules
open BoundsInst

let compute_ue_diff label v  =
  let ue = Option.get (TransitionLabel.update label v) in
  let ue_diff_syntactic =
    UpdateElement.sub ue (UpdateElement.of_var v)
  in

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
    if SMT.IncrementalZ3Solver.unsatisfiable s then UpdateElement.zero
    else ue_diff_syntactic



let compute_elcb program_vars ((gt,l),v) =
  Base.Set.to_sequence (GeneralTransition.transitions gt)
  |> Base.Sequence.filter ~f:(Location.equal l % Transition.target)
  |> Base.Sequence.map ~f:(fun t ->
      let label = Transition.label t in
      let ue_diff = compute_ue_diff label v in

      let ue_exp_abs_diff_bound =
        let temp_var_bound tv =
          let guard = Formulas.Formula.mk (TransitionLabel.guard label) in
          LocalSizeBound.find_bound program_vars tv guard (LocalSizeBound.c_range guard)
          |> Option.map (RealBound.of_intbound % LocalSizeBound.as_bound % Tuple2.first)
          |> Option.default RealBound.infinity
        in
        UpdateElement.exp_value_abs_bound ue_diff
        |> RealBound.substitute_f
            (fun v -> if Base.Set.mem program_vars v then RealBound.of_var v else temp_var_bound v)
      in

      RealBound.(of_constant (TransitionLabel.probability label) * ue_exp_abs_diff_bound )
    )
  |> RealBound.sum_sequence
