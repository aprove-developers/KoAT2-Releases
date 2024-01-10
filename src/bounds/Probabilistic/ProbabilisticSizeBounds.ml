open! OurBase
open ProbabilisticProgramModules
open Bounds
open Approximation.Probabilistic

let size_logger = Logging.(get ExpSize)

(** Get pre expected sizebounds. Should return [RationalBound.infinity] for temporary variables *)
let get_pre_size_exp program appr gt =
  if Program.is_initial_gt program gt then
    fun v ->
      if Set.mem (Program.input_vars program) v then
        RationalBound.of_var v
      else
        RationalBound.infinity
  else
    let start_loc = GeneralTransition.src gt in
    let pre_gt = Program.pre_gt program gt in
    fun v ->
      Set.to_sequence pre_gt
      |> Sequence.map ~f:(fun pre_gt -> ExpApproximation.sizebound appr (pre_gt, start_loc) v)
      |> RationalBound.sum


(** Get pre classical sizebounds. Should return [RationalBound.infinity] for temporary variables *)
let get_pre_size_classical program class_appr gt =
  if Program.is_initial_gt program gt then
    fun v ->
      if Set.mem (Program.input_vars program) v then
        RationalBound.of_var v
      else
        RationalBound.infinity
  else
    let start_loc = GeneralTransition.src gt in
    let pre_gt = Program.pre_gt program gt in
    fun v ->
      Set.to_sequence pre_gt
      |> Sequence.map ~f:(Set.to_sequence % GeneralTransition.transitions_to_target start_loc)
      |> Sequence.join
      |> Sequence.map ~f:(fun t -> ClassicalApproximation.sizebound class_appr t v)
      |> RationalBound.of_intbound % Bound.sum


type elcb_map = (GRV.t, RationalBound.t, GRV.comparator_witness) Map.t

let trivial_sizebounds program ~grvs_in_and_out elcbs class_appr appr =
  let trivial_sizebound_for_grv ((gt, l), v) =
    let get_pre_size_exp = get_pre_size_exp program appr gt in
    let get_pre_size_classical = get_pre_size_classical program class_appr gt in
    let var_overapprox = get_pre_size_exp v in
    let elcb = Map.find_exn elcbs ((gt, l), v) in
    let elcb_overapprox =
      BoundsHelper.RationalSubstHelper.substitute_bound_with_exp_and_class_sizes ~exp_subst:get_pre_size_exp
        ~class_subst:get_pre_size_classical elcb
    in
    let elcb_overapprox_plus_var = RationalBound.add var_overapprox elcb_overapprox in
    let propagated_bound =
      let combined_updates =
        Set.to_list (GeneralTransition.transitions_to_target l gt)
        |> List.map ~f:(fun (_, label, _) ->
               let open OptionMonad in
               let+ update = TransitionLabel.update label v in
               RationalBound.mul
                 (if label |> TransitionLabel.probability |> Polynomials.RationalLaurentPolynomial.is_const
                  then
                    label |> TransitionLabel.probability |> Polynomials.RationalLaurentPolynomial.get_constant
                    |> RationalBound.of_constant
                  else
                    (* TODO *)
                    RationalBound.infinity)
                 (UpdateElement.exp_value_abs_bound update))
        |> OptionMonad.sequence |> Option.map ~f:RationalBound.sum_list
      in
      match combined_updates with
      | Some combined_updates ->
          BoundsHelper.RationalSubstHelper.substitute_bound_with_exp_and_class_sizes
            ~exp_subst:get_pre_size_exp ~class_subst:get_pre_size_classical combined_updates
      | _ -> RationalBound.infinity
    in
    RationalBound.keep_simpler_bound elcb_overapprox_plus_var propagated_bound
    |> tap (fun r ->
           Logger.log size_logger Logger.DEBUG (fun () ->
               ( "trivial_sizebound_for_grv",
                 [
                   ("grv", GRV.to_id_string ((gt, l), v));
                   ("var_overapprox", RationalBound.to_string var_overapprox);
                   ("elcb", RationalBound.to_string elcb);
                   ("elcb_overapprox", RationalBound.to_string elcb_overapprox);
                   ("elcb_overapprox_plus_var", RationalBound.to_string elcb_overapprox_plus_var);
                   ("propagated_bound", RationalBound.to_string propagated_bound);
                   ("result", RationalBound.to_string r);
                 ] )))
  in
  List.fold
    ~f:(fun appr (rvt, v) -> ExpApproximation.add_sizebound (trivial_sizebound_for_grv (rvt, v)) rvt v appr)
    ~init:appr grvs_in_and_out


let nontrivial_sizebounds program ~program_vars ~scc ~rvts_scc elcbs class_appr appr =
  let entry_rvts = Sequence.to_list (BoundsHelper.entry_gts_with_locs program scc) in
  let get_pre_size_classical = get_pre_size_classical program class_appr in
  let nontrivial_sizebound appr v =
    let execute () =
      let start_value =
        Sequence.of_list entry_rvts
        |> Sequence.map ~f:(fun rvt -> ExpApproximation.sizebound appr rvt v)
        |> RationalBound.sum
        |> tap (fun s ->
               Logger.log size_logger Logger.DEBUG (fun () ->
                   ("start_value", [ ("res", RationalBound.to_string s) ])))
      in
      let acc_change_grv (gt, l) =
        let elcb = Map.find_exn elcbs ((gt, l), v) in
        let change_bound = RationalBound.substitute_f (get_pre_size_classical gt) elcb in
        RationalBound.(change_bound * ExpApproximation.timebound appr gt)
        |> tap (fun r ->
               Logger.log size_logger Logger.DEBUG (fun () ->
                   ( "acc_change_rvt",
                     [
                       ("rv", GRV.to_id_string ((gt, l), v));
                       ("elcb", RationalBound.to_string elcb);
                       ("res", RationalBound.to_string r);
                       ("change_bound", RationalBound.to_string change_bound);
                       ("time_bound", RationalBound.to_string (ExpApproximation.timebound appr gt));
                     ] )))
      in
      let acc_change = Sequence.of_list rvts_scc |> Sequence.map ~f:acc_change_grv |> RationalBound.sum in
      RationalBound.(start_value + acc_change)
    in
    Logger.with_log size_logger Logger.DEBUG
      (fun () ->
        ("nontrivial_sizebound", [ ("scc", GeneralTransitionSet.to_id_string scc); ("v", Var.to_string v) ]))
      ~result:RationalBound.to_string execute
  in
  Set.fold
    ~f:(fun appr v ->
      let new_bound = nontrivial_sizebound appr v in
      ExpApproximation.add_sizebounds new_bound (List.map ~f:(fun rvt -> (rvt, v)) rvts_scc) appr)
    program_vars ~init:appr


(** propagate sizes through nonprobabilistic updates *)
let propagate_sizes program ~program_vars ~rvts_scc class_appr appr =
  let propagate_once appr =
    (* Only propagate for scc rvs as trivial size bounds should handle propagation for incoming grvs *)
    let rvs_scc = List.cartesian_product rvts_scc (Set.to_list program_vars) in
    let propagate_for_grv appr ((gt, target_loc), v) =
      let transitions = Set.to_list (GeneralTransition.transitions_to_target target_loc gt) in
      let transitions_updates =
        List.map transitions ~f:(fun (_, t, _) -> TransitionLabel.update t v)
        |> List.map ~f:(Option.bind ~f:UpdateElement.to_polynomial)
        |> OptionMonad.sequence
      in
      (* Propagate bounds iff all transitions have linear non-probabilistic updates *)
      match transitions_updates with
      | Some update_polys ->
          let get_pre_size_exp = get_pre_size_exp program appr gt in
          let get_pre_size_classical = get_pre_size_classical program class_appr gt in
          let propagated_bound =
            Sequence.of_list update_polys
            |> Sequence.map ~f:(fun update_poly ->
                   RationalBound.of_intpoly update_poly
                   |> BoundsHelper.RationalSubstHelper.substitute_bound_with_exp_and_class_sizes
                        ~exp_subst:get_pre_size_exp ~class_subst:get_pre_size_classical)
            |> RationalBound.sum
          in
          if
            RationalBound.compare_asy propagated_bound (ExpApproximation.sizebound appr (gt, target_loc) v)
            < 0
          then (
            Logger.log size_logger Logger.DEBUG (fun () ->
                ( "propagate_sizes",
                  [
                    ("grv", GRV.to_id_string ((gt, target_loc), v));
                    ("new_bound", RationalBound.to_string propagated_bound);
                  ] ));
            MaybeChanged.changed (ExpApproximation.add_sizebound propagated_bound (gt, target_loc) v appr))
          else
            MaybeChanged.same appr
      | _ -> MaybeChanged.same appr
    in
    MaybeChanged.fold propagate_for_grv appr rvs_scc
  in
  Util.find_fixpoint propagate_once appr
