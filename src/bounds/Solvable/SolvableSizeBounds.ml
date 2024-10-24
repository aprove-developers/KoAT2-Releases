open Polynomials
open Bounds
open OurBase

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Check_Solvable = Check_Solvable.Make (Bounds.Bound) (PM)
  module Loop = Loop.Make (Bounds.Bound) (PM)
  module SimpleCycle = SimpleCycle.Make (Bounds.Bound) (PM)
  module Approximation = Approximation.MakeForClassicalAnalysis (Bounds.Bound) (PM)

  let heuristic_for_cycle appr program loop = Option.is_some @@ Check_Solvable.check_solvable loop

  module VT = struct
    type t = Transition.t * Var.t

    let equal (t1, v1) (t2, v2) = Transition.equal t1 t2 && Var.equal v1 v2
    let hash = Hashtbl.hash
  end

  module SizeBoundTable = Batteries.Hashtbl.Make (VT)

  let size_bound_table : (Transition.t * Bound.t) list option SizeBoundTable.t = SizeBoundTable.create 10

  (** Internal memoization: The idea is to use this cache if we applied cfr and
    1) delete it and use the original cache if we get a timeout or
    2) if the analysis of the unrolled scc is completed successfully use this cache as the main memory.
    TODO Currently, we just reset the cache. *)
  let reset_cfr () = SizeBoundTable.clear size_bound_table

  let lift appr t var = function
    | None -> Bound.infinity
    | Some xs ->
        List.map xs ~f:(fun (entry, local_size) ->
            Bound.substitute_f (Approximation.sizebound appr entry) local_size)
        |> Sequence.of_list |> Bound.sum


  let improve_solvable lift_var appr program trans t var =
    let loops_opt =
      SimpleCycle.find_loop
        ~relevant_vars:(Option.some @@ VarSet.singleton var)
        heuristic_for_cycle appr program trans t
    in
    if Option.is_some loops_opt then
      let loop, handled_transitions, entries_traversal = Option.value_exn loops_opt in
      let loop_red =
        Loop.eliminate_non_contributors ~relevant_vars:(Option.some @@ VarSet.singleton var) loop
      in
      let closed_forms = Check_Solvable.compute_closed_form loop_red
      and time_bound =
        let global_bound = Bound.sum_list (List.map handled_transitions ~f:(Approximation.timebound appr)) in
        if Bound.is_linear global_bound then
          global_bound
        else
          Bound.min_asy global_bound (MultiphaseRankingFunction.time_bound loop 5)
      in
      if Bound.is_finite time_bound then (
        List.iter
          ~f:(fun (var, pe) ->
            let local_bound =
              Bound.max
                (Loop.compute_bound_n_iterations loop_red var
                   (max 0 ((OurInt.to_int @@ PolyExponential.ComplexPE.max_const pe) - 1)))
                (PolyExponential.ComplexPE.to_bound pe)
            in
            let res =
              List.map
                ~f:(fun (entry, traversal) ->
                  ( entry,
                    Bound.substitute (Var.of_string "n") ~replacement:time_bound local_bound
                    |> Bound.substitute_f (fun var ->
                           Bound.of_poly @@ (Map.find traversal var |? Polynomial.of_var var)) ))
                entries_traversal
              |> Option.some
            in
            SizeBoundTable.add size_bound_table (t, var) res)
          (Option.value_exn closed_forms);
        Set.fold (Loop.vars loop_red) ~init:appr ~f:lift_var)
      else
        appr
    else
      appr


  let improve_commutive appr program trans t var =
    let loops_opt = SimpleCycle.find_commuting_loops heuristic_for_cycle appr program trans t in
    if Option.is_some loops_opt then
      let loops, loop_trans, handled_transitions, partial_evaluation = Option.value_exn loops_opt in
      let loop_vars =
        Set.inter (Program.vars program)
          (Set.union_list (module Var) (List.map ~f:(Loop.vars % Tuple2.first) loops))
      in
      let initial_substitution =
        Map.of_alist_exn
          (module Var)
          (List.zip_exn (Set.to_list loop_vars) (Set.to_list loop_vars |> List.map ~f:Bound.of_var))
      in
      let bound =
        List.fold ~init:initial_substitution loops ~f:(fun substitution (loop, handled_transitions) ->
            let closed_forms = Option.value_exn @@ Check_Solvable.compute_closed_form loop in
            let time_bound =
              Bound.sum_list (List.map handled_transitions ~f:(Approximation.timebound appr))
            in
            let new_substitution =
              List.map closed_forms
                ~f:
                  (Tuple2.map2 (fun pe ->
                       let local_bound =
                         Bound.max
                           (Loop.compute_bound_n_iterations loop var
                              (max 0 ((OurInt.to_int @@ PolyExponential.ComplexPE.max_const pe) - 1)))
                           (PolyExponential.ComplexPE.to_bound pe)
                         |> Bound.substitute (Var.of_string "n") ~replacement:time_bound
                       in
                       local_bound))
            in
            Map.map ~f:(Bound.substitute_all @@ Map.of_alist_exn (module Var) new_substitution) substitution
            |> Map.map
                 ~f:
                   (Bound.substitute_f (fun var ->
                        Bound.of_poly @@ (Map.find partial_evaluation var |? Polynomial.of_var var))))
      in
      Map.fold bound ~init:appr ~f:(fun ~key:var ~data:local_bound appr ->
          let entries = Program.entry_transitions program handled_transitions in
          let bound =
            lift appr t var (Option.some @@ List.map entries ~f:(fun entry -> (entry, local_bound)))
          in
          List.fold loop_trans ~init:appr ~f:(fun appr t -> Approximation.add_sizebound bound t var appr))
    else
      appr


  let improve_t ?(commuting = false) program trans t appr =
    let lift_var appr var =
      let lifted_bound = lift appr t var (SizeBoundTable.find size_bound_table (t, var)) in
      Approximation.add_sizebound lifted_bound t var appr
    in
    Set.fold
      ~f:(fun appr var ->
        if
          (not commuting)
          && SizeBoundTable.mem size_bound_table (t, var)
          && (not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var)
        then
          lift_var appr var
        else if not @@ Bound.is_linear @@ Approximation.sizebound appr t var then
          if commuting then
            improve_commutive appr program trans t var
          else
            improve_solvable lift_var appr program trans t var
        else
          appr)
      (TransitionLabel.input_vars (Transition.label t))
      ~init:appr


  let improve ?(commuting = false) program ?(scc = None) appr =
    let trans = scc |? Program.transitions program in
    Set.fold ~f:(flip @@ improve_t ~commuting program trans) trans ~init:appr
end
