open OurBase
open Bounds

let logger = Logging.(get Size)

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module LSB = LocalSizeBound.Make (TransitionLabel) (Transition) (Program)
  module RV = RVGTypes.MakeRV (TransitionLabel) (Transition)
  module RVG = RVGTypes.MakeRVG (PM)

  (* Computes size bounds for SCCs with negation. Uses the original KoAT method, and only considers bounds on absolute values
     *)
  let compute_ (rvg : RVG.t) (get_lsb : RV.t -> LSB.t * bool) (get_timebound : Transition.t -> Bound.t)
      (get_sizebound : Transition.t -> Var.t -> Bound.t) (scc : RV.t List.t) =
    let scc_rvset = Set.of_list (module RV) scc in
    let rvs_equality, rvs_non_equality = List.partition_tf ~f:(Tuple2.second % get_lsb) scc in

    (* All transitions that are present in the scc and that are not of equality type.
       Corresponds to T_C in the thesis. *)
    let transitions =
      rvs_non_equality
      |> List.map ~f:(fun (t, v) -> t)
      |> TransitionSet.stable_dedup_list
      |> tap (fun transitions ->
             Logger.log logger Logger.DEBUG (fun () ->
                 ( "transitions",
                   [
                     ( "result",
                       Util.sequence_to_string ~f:Transition.to_id_string (Sequence.of_list transitions) );
                   ] )))
    in

    (* Returns all the variables with which the given transition does occur as result variable in the scc. *)
    let get_scc_vars transition =
      rvs_non_equality
      |> List.filter ~f:(fun (t, v) -> Transition.equal t transition)
      |> List.map ~f:(fun (t, v) -> v)
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

    (* Returns all result variables that may influence the given result variable from within the scc.
        Corresponds to V_rv in the thesis. *)
    let scc_variables rv =
      rv |> Set.to_sequence % pre_in_scc
      |> Sequence.map ~f:(fun (t, v) -> v)
      |> Sequence.of_list % VarSet.stable_dedup_list % Sequence.to_list
    in

    let starting_value =
      let rvs_equality_type_max_constant =
        List.map ~f:(LSB.constant % Tuple2.first % get_lsb) rvs_equality |> List.fold ~f:max ~init:0
      in
      scc
      |> Set.to_sequence % Set.union_list (module RV) % List.map ~f:pre_out_scc
      |> Sequence.map ~f:(uncurry get_sizebound)
      |> Bound.sum
      |> Bound.add (Bound.of_int rvs_equality_type_max_constant)
    in

    let transition_scaling_factor t =
      let affecting_variables =
        get_scc_vars t
        |> List.map ~f:(fun v -> scc_variables (t, v))
        |> List.map ~f:Sequence.length |> List.max_elt ~compare:Int.compare |? 1
      in

      let scaling_explicit =
        t |> get_scc_vars
        |> List.map ~f:(fun v -> Tuple2.first @@ get_lsb (t, v))
        |> List.map ~f:LSB.factor |> List.max_elt ~compare:Int.compare |? 1
        |> tap (fun result ->
               Logger.log logger Logger.DEBUG (fun () ->
                   ("extreme_scaling_factor", [ ("result", Int.to_string result) ])))
      in

      OurInt.of_int (scaling_explicit * affecting_variables)
    in

    let loop_scaling =
      Sequence.of_list transitions
      |> Sequence.map ~f:(fun t ->
             let scaling = transition_scaling_factor t in
             if OurInt.(equal scaling one) then
               Bound.one
             else
               Bound.exp scaling (get_timebound t))
      |> Bound.product
    in

    let incoming_constant rv v =
      Set.to_sequence (pre_out_scc rv)
      |> Sequence.filter ~f:(fun (_, v') -> Var.equal v v')
      |> Sequence.map ~f:(uncurry get_sizebound)
      |> Bound.sum
    in

    let rv_constant = Bound.of_int % LSB.constant % Tuple2.first % get_lsb in

    let rv_effect rv =
      let rv_vars =
        Set.diff (LSB.vars @@ Tuple2.first @@ get_lsb rv) (VarSet.of_sequence @@ scc_variables rv)
        |> Set.to_sequence
      in
      Bound.(rv_constant rv + (Sequence.map ~f:(incoming_constant rv) rv_vars |> sum))
    in

    let transition_effect t = get_scc_vars t |> List.map ~f:(fun v -> rv_effect (t, v)) |> Bound.sum_list in

    let loop_effect =
      Sequence.of_list transitions
      |> Sequence.map ~f:(fun t ->
             if Bound.is_infinity (get_timebound t) then
               if Bound.(equal zero (transition_effect t)) then
                 Bound.zero
               else
                 Bound.infinity
             else
               Bound.(get_timebound t * transition_effect t))
      |> Bound.sum
    in

    (if Bound.(is_infinity (starting_value + loop_effect)) then
       Bound.infinity
     else if Bound.is_infinity loop_scaling && Bound.(equal zero (starting_value + loop_effect)) then
       Bound.zero
     else
       (* We have computed a bound in the absolute values*)
       Bound.(loop_scaling * (starting_value + loop_effect)))
    |> tap (fun res ->
           Logger.log logger Logger.DEBUG (fun () ->
               ( "compute",
                 [
                   ("loop_scaling", Bound.to_string loop_scaling);
                   ("starting_value", Bound.to_string starting_value);
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
      | Some get_lsb -> compute_ rvg get_lsb get_timebound get_sizebound scc
      | None -> Bound.infinity
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("compute_nontrivial_bound", [ ("scc", RVG.rvs_to_id_string scc) ]))
      ~result:Bound.to_string execute
end
