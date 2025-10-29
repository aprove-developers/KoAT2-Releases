open! OurBase
open Bounds

let logger = Logging.(get Size)

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module LSB = LocalSizeBound.Make (TransitionLabel) (Transition) (RV) (Program)
  module RV = PM.RV
  module RVG = RVGTypes.MakeRVG (PM)

  (* Computes size bounds for SCCs with negation. Uses the original KoAT method, and only considers bounds on absolute values
     *)
  let compute_ program (rvg : RVG.t) (get_lsb : RV.t -> LSB.t_rec * bool)
      (get_timebound : Transition.t -> Bound.t) (get_sizebound : RV.modifier -> Var.t -> Bound.t)
      (scc : RV.t List.t) =
    let scc_rvset = Set.of_list (module RV) scc in
    let rvs_equality, rvs_non_equality = List.partition_tf ~f:(Tuple2.second % get_lsb) scc in

    (* All transitions that are present in the scc and that are not of equality type.
       Corresponds to T_C in the thesis. *)
    let transitions =
      rvs_non_equality
      |> List.filter_map ~f:(fun rv ->
             if RV.has_transition rv then
               Option.return (RV.transition rv)
             else
               None)
      |> TransitionSet.stable_dedup_list
      |> tap (fun transitions ->
             Logger.log logger Logger.DEBUG (fun () ->
                 ( "transitions",
                   [
                     ( "result",
                       Util.sequence_to_string ~f:Transition.to_id_string (Sequence.of_list transitions) );
                   ] )))
    in

    let function_calls =
      rvs_non_equality
      |> List.filter_map ~f:(fun rv ->
             if RV.has_transition rv then
               None
             else
               Option.return (RV.function_call rv))
      |> VarFunctionCallSet.of_list
      |> tap (fun function_calls ->
             Logger.log logger Logger.DEBUG (fun () ->
                 ("function_calls", [ ("result", VarFunctionCallSet.to_string function_calls) ])))
    in

    let modifiers =
      let modifiers_of_trans = transitions |> List.map ~f:RV.modifier_of_transition in
      let modifiers_of_fcs = function_calls |> Set.to_list |> List.map ~f:RV.modifier_of_function_call in
      List.append modifiers_of_trans modifiers_of_fcs
    in

    (* Returns all the variables with which the given transition does occur as result variable in the scc. *)
    let get_scc_vars modifier =
      rvs_non_equality
      |> List.filter ~f:(fun (m, _) -> RV.equal_modifier m modifier)
      |> List.map ~f:(fun (_, v) -> v)
      |> VarSet.stable_dedup_list
      |> tap (fun scc_vars ->
             Logger.log logger Logger.DEBUG (fun () ->
                 ( "scc_vars",
                   [ ("result", Util.sequence_to_string ~f:Var.to_string (Sequence.of_list scc_vars)) ] )))
    in

    (* Returns all result variables that may influence the given result variable and that are part of the scc. *)
    let pre_in_scc (rv : RV.t) = rv |> RVG.pre rvg |> Set.of_list (module RV) |> Set.inter scc_rvset in

    (* Returns all result variables that may influence the given result variable and that are not part of the scc. *)
    let pre_out_scc rv = rv |> RVG.pre rvg |> Set.of_list (module RV) |> fun pre -> Set.diff pre scc_rvset in

    (* Returns all result variables that may influence the given result variable and that are part of the scc. *)
    let pre_omega_in_scc (rv : RV.t) =
      rv |> RVG.pre_omega rvg |> Set.of_list (module RV) |> Set.inter scc_rvset
    in

    (* Returns all result variables that may influence the given result variable and that are not part of the scc. *)
    let pre_omega_out_scc rv =
      rv |> RVG.pre_omega rvg |> Set.of_list (module RV) |> fun pre -> Set.diff pre scc_rvset
    in

    (* Returns all result variables that may influence the given result variable from within the scc.
        Corresponds to V_rv in the thesis. *)
    let scc_variables rv =
      rv |> Set.to_sequence % pre_in_scc
      |> Sequence.map ~f:(fun (t, v) -> v)
      |> Sequence.of_list % VarSet.stable_dedup_list % Sequence.to_list
    in

    (* Returns all result variables that may influence the given result variable from within the scc.
    Corresponds to F_rv in the thesis. *)
    let scc_variables_omega rv =
      rv |> Set.to_sequence % pre_omega_in_scc
      |> Sequence.map ~f:(fun (t, v) -> v)
      |> Sequence.of_list % VarSet.stable_dedup_list % Sequence.to_list
    in

    let starting_value =
      let rvs_equality_type_max_constant =
        List.map ~f:(LSB.constant_rec % Tuple2.first % get_lsb) rvs_equality |> List.fold ~f:max ~init:0
      in
      scc
      |> Set.to_sequence % Set.union_list (module RV) % List.map ~f:pre_out_scc
      |> Sequence.map ~f:(uncurry get_sizebound)
      |> Bound.sum
      |> Bound.add (Bound.of_int rvs_equality_type_max_constant)
    in

    let starting_value_omega =
      let rvs_equality_type_max_constant =
        List.map ~f:(LSB.constant_rec % Tuple2.first % get_lsb) rvs_equality |> List.fold ~f:max ~init:0
      in
      scc
      |> Set.to_sequence % Set.union_list (module RV) % List.map ~f:pre_omega_out_scc
      |> Sequence.map ~f:(uncurry get_sizebound)
      |> Bound.maximum
      |> Bound.max (Bound.of_int rvs_equality_type_max_constant)
    in

    let modifier_scaling_factor m =
      let affecting_variables =
        get_scc_vars m
        |> List.map ~f:(fun v -> scc_variables (m, v))
        |> List.map ~f:Sequence.length |> List.max_elt ~compare:Int.compare |? 1
      in
      let affecting_variables_omega =
        get_scc_vars m
        |> List.map ~f:(fun v -> scc_variables_omega (m, v))
        |> List.map ~f:Sequence.length |> List.max_elt ~compare:Int.compare |? 1
      in

      let insertSB m bound =
        Program.input_vars program |> Set.to_list
        |> Set.to_sequence % Set.union_list (module RV) % List.map ~f:(fun v -> pre_out_scc (m, v))
        |> Sequence.map ~f:(fun rv -> Bound.substitute_f (get_sizebound (RV.modifier rv)) bound)
        |> Bound.maximum
      in

      let scaling_explicit =
        get_scc_vars m
        |> List.map ~f:(fun v -> Tuple2.first @@ get_lsb (m, v))
        |> List.map ~f:(insertSB m % Bound.of_poly % LSB.factor_rec)
        |> Sequence.of_list |> Bound.maximum
        |> Bound.(max one)
        |> tap (fun result ->
               Logger.log logger Logger.DEBUG (fun () ->
                   ("extreme_scaling_factor", [ ("result", Bound.to_string result) ])))
      in

      Bound.(scaling_explicit * (of_int affecting_variables + of_int affecting_variables_omega))
    in

    let loop_scaling =
      Sequence.of_list modifiers
      |> Sequence.map ~f:(fun m ->
             let scaling = modifier_scaling_factor m in
             if Bound.(equal scaling one) then
               Bound.one
             else if RV.is_transition m then
               Bound.(exp scaling (get_timebound (RV.transition_ m)))
             else
               let timebound =
                 List.filter_map
                   (Set.to_list @@ Program.transitions program)
                   ~f:(fun t ->
                     if Transition.has_rec_call t (RV.function_call_ m) then
                       Option.return @@ get_timebound t
                     else
                       None)
                 |> Bound.sum_list
               in
               Bound.(exp scaling timebound))
      |> Bound.product
    in

    let incoming_constant rv v =
      Set.to_sequence (pre_out_scc rv)
      |> Sequence.filter ~f:(fun (_, v') -> Var.equal v v')
      |> Sequence.map ~f:(uncurry get_sizebound)
      |> Bound.sum
    in

    let incoming_constant_omega rv v =
      Set.to_sequence (pre_omega_out_scc rv)
      |> Sequence.filter ~f:(fun (_, v') -> Var.equal v v')
      |> Sequence.map ~f:(uncurry get_sizebound)
      |> Bound.sum
    in

    let rv_constant = Bound.of_int % LSB.constant_rec % Tuple2.first % get_lsb in

    let rv_effect rv =
      let actV =
        LSB.vars_rec @@ Tuple2.first @@ get_lsb rv
        |> Set.to_list
        |> List.filter_map ~f:(fun v ->
               if VarFunctionCall.is_function_call v then
                 None
               else
                 Option.return (VarFunctionCall.to_var v))
        |> VarSet.of_list
      in
      let actF =
        LSB.vars_rec @@ Tuple2.first @@ get_lsb rv |> Set.filter ~f:VarFunctionCall.is_function_call
      in
      let rv_vars_actV = Set.diff actV (VarSet.of_sequence @@ scc_variables rv) |> Set.to_sequence in
      let rv_vars_actF =
        Set.diff
          (Set.map (module Var) ~f:VarFunctionCall.return_var actF)
          (VarSet.of_sequence @@ scc_variables rv)
        |> Set.to_sequence
      in
      Bound.(
        rv_constant rv
        + (Sequence.map ~f:(incoming_constant rv) rv_vars_actV |> sum)
        + (Sequence.map ~f:(incoming_constant_omega rv) rv_vars_actF |> sum))
    in

    let modifier_effect m = get_scc_vars m |> List.map ~f:(fun v -> rv_effect (m, v)) |> Bound.sum_list in

    let loop_effect =
      Sequence.of_list modifiers
      |> Sequence.map ~f:(fun m ->
             let timebound =
               if RV.is_transition m then
                 get_timebound (RV.transition_ m)
               else
                 List.filter_map transitions ~f:(fun t ->
                     if Transition.has_rec_call t (RV.function_call_ m) then
                       Option.return @@ get_timebound t
                     else
                       None)
                 |> Bound.sum_list
             in
             if Bound.is_infinity timebound then
               if Bound.(equal zero (modifier_effect m)) then
                 Bound.zero
               else
                 Bound.infinity
             else
               Bound.(timebound * modifier_effect m))
      |> Bound.sum
    in

    (if Bound.(is_infinity (starting_value + starting_value_omega + loop_effect)) then
       Bound.infinity
     else if
       Bound.is_infinity loop_scaling
       && Bound.(equal zero (starting_value + starting_value_omega + loop_effect))
     then
       Bound.zero
     else
       (* We have computed a bound in the absolute values*)
       Bound.(loop_scaling * (starting_value + starting_value_omega + loop_effect)))
    |> tap (fun res ->
           Logger.log logger Logger.DEBUG (fun () ->
               ( "compute",
                 [
                   ("loop_scaling", Bound.to_string loop_scaling);
                   ("starting_value", Bound.(to_string @@ (starting_value + starting_value_omega)));
                   ("loop_effect", Bound.to_string loop_effect);
                   ("result", Bound.to_string res);
                 ] )))


  (* Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
     Corresponds to 'SizeBounds for nontrivial SCCs'. *)
  let compute program rvg get_timebound get_sizebound scc get_lsb =
    let lsb_fun =
      let lsbs = List.map ~f:(fun (t, v) -> ((t, v), get_lsb (t, v))) scc in
      if List.for_all ~f:(Option.is_some % Tuple2.second) lsbs then
        Some
          (fun k -> (Tuple2.map2 Lazy.force % Option.value_exn) @@ List.Assoc.find_exn lsbs ~equal:RV.equal k)
      else
        None
    in

    let execute () =
      match lsb_fun with
      | Some get_lsb -> compute_ program rvg get_lsb get_timebound get_sizebound scc
      | None -> Bound.infinity
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("compute_nontrivial_bound", [ ("scc", RVG.rvs_to_id_string scc) ]))
      ~result:Bound.to_string execute
end
