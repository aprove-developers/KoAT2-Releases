open Batteries
open BoundsInst

let logger = Logging.(get Size)

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module LSB = LocalSizeBound.Make(TransitionLabel)(Transition)(Program)
  module RV = RVGTypes.MakeRV(TransitionLabel)(Transition)
  module RVG = RVGTypes.MakeRVG(PM)

  module RVSet = Set.Make(RV.RVTuple_)

  (* Computes size bounds for SCCs with negation. Uses the original KoAT method, and only considers bounds on absolute values
   * *)
  let compute_
        (rvg: RVG.t)
        (get_lsb: RV.t -> LSB.t * bool)
        (get_timebound: Transition.t -> Bound.t)
        (get_sizebound: Transition.t -> Var.t -> Bound.t)
        (scc: RV.t List.t) =

    let scc_rvset = RVSet.of_list scc in
    let (rvs_equality, rvs_non_equality) = List.partition (Tuple2.second % get_lsb) scc in

    (** All transitions that are present in the scc and that are not of equality type.
        Corresponds to T_C in the thesis. *)
    let transitions =
      rvs_non_equality
      |> List.map (fun (t,v) -> t)
      |> List.unique ~eq:Transition.same
      |> tap (fun transitions -> Logger.log logger Logger.DEBUG (fun () -> "transitions", ["result", Util.enum_to_string Transition.to_id_string (List.enum transitions)]))
    in

    (** Returns all the variables with which the given transition does occur as result variable in the scc. *)
    let get_scc_vars transition =
      rvs_non_equality
      |> List.filter (fun (t,v) -> Transition.same t transition)
      |> List.map (fun (t,v) -> v)
      |> List.unique ~eq:Var.equal
      |> tap (fun scc_vars -> Logger.log logger Logger.DEBUG (fun () -> "scc_vars", ["result", Util.enum_to_string Var.to_string (List.enum scc_vars)]))
      |> List.enum
    in

    (** Returns all result variables that may influence the given result variable and that are part of the scc. *)
    let pre_in_scc rv =
      rv
      |> RVG.pre rvg
      |> RVSet.of_enum
      |> RVSet.inter scc_rvset
    in

    (** Returns all result variables that may influence the given result variable and that are not part of the scc. *)
    let pre_out_scc rv =
      rv
      |> RVG.pre rvg
      |> RVSet.of_enum
      |> fun pre -> RVSet.diff pre scc_rvset
    in

    (** Returns all result variables that may influence the given result variable from within the scc.
        Corresponds to V_rv in the thesis. *)
    let scc_variables rv =
      rv
      |> RVSet.enum % pre_in_scc
      |> Enum.map (fun (t,v) -> v)
      |> Enum.uniq_by Var.equal
    in


    let starting_value =
      let rvs_equality_type_max_constant =
        List.map (LSB.constant % Tuple2.first % get_lsb) rvs_equality
        |> List.fold_left max 0
      in
      List.enum scc
      |> RVSet.enum % Enum.fold RVSet.union RVSet.empty % Enum.map pre_out_scc
      |> Enum.map (uncurry get_sizebound)
      |> Bound.sum
      |> Bound.add (Bound.of_int rvs_equality_type_max_constant)
    in

    let transition_scaling_factor t =
      let affecting_variables =
        get_scc_vars t
        |> Enum.map (fun v -> scc_variables (t,v))
        |> Enum.map Enum.count
        |> Util.max_option (>)
        |? 1
      in

      let scaling_explicit =
        t
        |> get_scc_vars
        |> Enum.map (fun v -> Tuple2.first @@ get_lsb (t,v))
        |> Enum.map LSB.factor
        |> Util.max_option (>)
        |? 1
        |> tap (fun result -> Logger.log logger Logger.DEBUG (fun () -> "extreme_scaling_factor", ["result", Int.to_string result]))
      in

      OurInt.of_int (scaling_explicit * affecting_variables)
    in

    let loop_scaling =
      List.enum transitions
      |> Enum.map (fun t ->
            let scaling = transition_scaling_factor t in
            if (OurInt.(equal scaling one)) then Bound.one else Bound.exp scaling (get_timebound t)
         )
      |> Bound.product
    in

    let incoming_constant rv v =
      RVSet.enum (pre_out_scc rv)
      |> Enum.filter (fun (_,v') -> Var.equal v v')
      |> Enum.map (uncurry get_sizebound)
      |> Bound.sum
    in

    let rv_constant = Bound.of_int % LSB.constant % Tuple2.first % get_lsb in

    let rv_effect rv =
      let rv_vars =
        Tuple2.first (get_lsb rv)
        |> VarSet.enum % LSB.vars
        |> Util.without Var.equal (scc_variables rv)
      in
      Bound.(rv_constant rv + (Enum.map (incoming_constant rv) rv_vars |> sum))
    in

    let transition_effect t =
      get_scc_vars t
      |> Enum.map (fun v -> rv_effect (t,v))
      |> Bound.sum
    in

    let loop_effect =
      List.enum transitions
      |> Enum.map (fun t ->
             if Bound.is_infinity (get_timebound t) then
               if Bound.(equal zero (transition_effect t)) then
                 Bound.zero
               else
                 Bound.infinity
             else
               Bound.(get_timebound t * transition_effect t)
           )
      |> Bound.sum
    in


    (if Bound.(is_infinity (starting_value + loop_effect )) then
      Bound.infinity
    else if (Bound.is_infinity loop_scaling) && Bound.(equal zero (starting_value + loop_effect)) then
      Bound.zero
    else
      (* We have computed a bound in the absolute values*)
      Bound.(loop_scaling * (starting_value + loop_effect)))
      |> tap (fun res -> Logger.log logger Logger.DEBUG (fun () -> "compute",[ "loop_scaling", Bound.to_string loop_scaling
                                                                             ; "starting_value", Bound.to_string starting_value
                                                                             ; "loop_effect", Bound.to_string loop_effect
                                                                             ; "result", Bound.to_string res]))


  (** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
      Corresponds to 'SizeBounds for nontrivial SCCs'. *)
  let compute program rvg get_timebound get_sizebound scc get_lsb =
    let lsb_fun =
      let lsbs = List.map (fun(t,v) -> (t,v), get_lsb (t,v)) scc in
      if List.for_all (Option.is_some % Tuple2.second) lsbs then
        Some (fun k -> Tuple2.map2 Lazy.force % Option.get @@ List.assoc k lsbs)
      else None
    in

    let execute () =
      match lsb_fun with
      | Some get_lsb -> compute_ rvg get_lsb get_timebound get_sizebound scc
      | None ->  Bound.infinity
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "compute_nontrivial_bound", ["scc", RVG.rvs_to_id_string scc])
                       ~result:Bound.to_string
                       execute
end
