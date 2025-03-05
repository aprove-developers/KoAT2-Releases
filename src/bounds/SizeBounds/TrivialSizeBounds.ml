open! OurBase
(** Modules used to infer size-bounds for trivial components. *)

open Bounds
open PolyRec

(** Modules used to infer size-bounds for trivial components. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'.*)

(** Logger Size *)
let logger = Logging.(get Size)

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module RV = PM.RV
  module RVG = RVGTypes.MakeRVG (PM)

  (** Returns the maximum of all incoming sizebounds applied to the local sizebound.
      Corresponds to 'SizeBounds for trivial SCCs':
      S'(alpha) = max(S_l(alpha)(S(t',v_1),...,S(t',v_n)) for all t' in pre(t)). *)
  let incoming_bound pre (get_sizebound : RV.modifier -> Var.t -> Bound.t) lsb (m : RV.modifier) v =
    (* since this is a trivial scc*)
    let execute () =
      (* If the LSB is constant there are no pre-transitions in the RVG *)
      if Bound.is_constant lsb then
        lsb
      else
        let substitute_with_prevalues t' = Bound.substitute_f (fun v -> get_sizebound t' v) lsb in
        pre |> Sequence.map ~f:substitute_with_prevalues |> Bound.maximum
    in
    Logger.with_log logger Logger.DEBUG
      (fun () ->
        ("compute_highest_incoming_bound", [ ("lsb", Bound.to_string lsb); ("rv", RV.to_id_string (m, v)) ]))
      ~result:Bound.to_string execute


  let incoming_bound_lsb program (get_sizebound : RV.modifier -> Var.t -> Bound.t) lsb rv v =
    let pre_transitions =
      if Set.is_empty (Bound.vars lsb) then
        Sequence.empty
      else
        Sequence.of_list
        @@ List.map ~f:RV.modifier_of_transition (Set.to_list @@ Program.pre program (RV.transition rv))
    in
    incoming_bound pre_transitions get_sizebound lsb (RV.modifier rv) v


  let incoming_bound_lifted_update program get_sizebound upd t v =
    let pre_transitions =
      Sequence.of_list @@ List.map ~f:RV.modifier_of_transition (Set.to_list @@ Program.pre program t)
    in
    incoming_bound pre_transitions get_sizebound upd (RV.modifier_of_transition t) v


  let incoming_bound_lifted_update_fc program get_sizebound upd fc v =
    let pre_transitions t =
      Sequence.of_list @@ List.map ~f:RV.modifier_of_transition (Set.to_list @@ Program.pre program t)
    in
    let pre_transitions =
      List.filter_map
        (Set.to_list @@ Program.transitions program)
        ~f:(fun t ->
          if Set.mem (Transition.rec_vars t) fc then
            Option.return @@ pre_transitions t
          else
            None)
      |> Sequence.of_list |> Sequence.join
    in
    if Sequence.is_empty pre_transitions then
      upd
    else
      incoming_bound pre_transitions get_sizebound upd (RV.modifier_of_function_call fc) v


  let subRecSize program (get_sizebound : RV.modifier -> Var.t -> Bound.t) rec_v =
    let open VarRec in
    match rec_v with
    | Recursion (loc, var, map) ->
        let transitions_ending_return_locations =
          Set.inter (Program.reachable_locations program loc) (Program.return_locations program)
          |> Set.to_list
          |> List.map ~f:(Program.ending_in_loc program)
          |> List.map ~f:Set.to_list |> List.concat |> TransitionSet.of_list
        in
        Bound.maximum
          (Sequence.of_list
          @@ List.map
               ~f:(fun t -> get_sizebound (RV.modifier_of_transition t) (VarRec.return_var rec_v))
               (Set.to_list transitions_ending_return_locations))
    | Var _ -> Bound.of_var (VarRec.to_var rec_v)
    | Argument _ -> Bound.of_var (VarRec.to_var rec_v)
    | _ -> Bound.infinity


  let obtainPolyFromFCs program get_sizebound =
    PolyRec.fold ~const:Bound.of_constant ~plus:Bound.add ~times:Bound.mul ~pow:Bound.pow
      ~indeterminate:(subRecSize program get_sizebound)


  (** Computes a bound for a  scc. That is an scc which consists only of one result variable without a loop to itself.
      Corresponds to 'SizeBounds for trivial SCCs'. *)
  let compute (program : Program.t) (get_sizebound : RV.modifier -> Var.t -> Bound.t) ((m, v) : PM.RV.t)
      (lsb_as_bound : PolyRec.t Option.t) =
    let open OptionMonad in
    let execute () =
      if RV.has_transition (m, v) then
        let t = RV.transition_ m in
        let res_from_lsb, res_from_update =
          if Program.is_initial program t then
            let res_from_lsb =
              if Option.is_some lsb_as_bound then
                obtainPolyFromFCs program get_sizebound (Option.value_exn lsb_as_bound)
              else
                Bound.infinity
            in
            let res_from_update =
              let+ update = TransitionLabel.update (Transition.label t) v in
              if Set.is_subset (PolyRec.vars update) ~of_:(TransitionLabel.input_vars (Transition.label t))
              then
                obtainPolyFromFCs program get_sizebound update
              else
                Bound.infinity
            in
            (res_from_lsb, res_from_update)
          else
            let res_from_lsb =
              if Option.is_some lsb_as_bound then
                incoming_bound_lsb program get_sizebound
                  (obtainPolyFromFCs program get_sizebound (Option.value_exn lsb_as_bound))
                  (m, v) v
              else
                Bound.infinity
            in
            let res_from_update =
              let+ update = TransitionLabel.update (Transition.label t) v in
              if Set.is_subset (PolyRec.vars update) ~of_:(TransitionLabel.input_vars (Transition.label t))
              then
                let lsb = obtainPolyFromFCs program get_sizebound update in
                incoming_bound_lifted_update program get_sizebound lsb t v
              else
                Bound.infinity
            in
            (res_from_lsb, res_from_update)
        in
        Bound.(keep_simpler_bound res_from_lsb (res_from_update |? infinity))
      else
        let update = RV.update (m, v) v in
        if Set.is_subset (PolyRec.vars update) ~of_:(Program.input_vars program) then
          let lsb = obtainPolyFromFCs program get_sizebound update in
          incoming_bound_lifted_update_fc program get_sizebound lsb (RV.function_call (m, v)) v
        else
          Bound.infinity
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("compute_trivial_bound", [ ("rv", RV.to_id_string (m, v)) ]))
      ~result:Bound.to_string execute
end
