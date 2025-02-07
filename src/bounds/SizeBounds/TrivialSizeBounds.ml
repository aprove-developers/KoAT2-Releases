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
  module RV = RVGTypes.MakeRV (PM.TransitionLabel) (PM.Transition)
  module RVG = RVGTypes.MakeRVG (PM)

  (** Returns the maximum of all incoming sizebounds applied to the local sizebound.
      Corresponds to 'SizeBounds for trivial SCCs':
      S'(alpha) = max(S_l(alpha)(S(t',v_1),...,S(t',v_n)) for all t' in pre(t)). *)
  let incoming_bound pre_transitions get_sizebound lsb t v =
    (* since this is a trivial scc*)
    let execute () =
      (* If the LSB is constant there are no pre-transitions in the RVG *)
      if Bound.is_constant lsb then
        lsb
      else
        let substitute_with_prevalues t' = Bound.substitute_f (fun v -> get_sizebound t' v) lsb in
        pre_transitions |> Sequence.map ~f:substitute_with_prevalues |> Bound.max_seq
    in
    Logger.with_log logger Logger.DEBUG
      (fun () ->
        ( "compute_highest_incoming_bound",
          [ ("lsb", Bound.to_string lsb); ("transition", Transition.to_id_string t) ] ))
      ~result:Bound.to_string execute


  let incoming_bound_lsb program get_sizebound lsb t v =
    let pre_transitions =
      if Set.is_empty (Bound.vars lsb) then
        Sequence.empty
      else
        Set.to_sequence (Program.pre program t)
    in
    incoming_bound pre_transitions get_sizebound lsb t v


  let incoming_bound_lifted_update program get_sizebound upd t v =
    let pre_transitions = Set.to_sequence (Program.pre program t) in
    incoming_bound pre_transitions get_sizebound upd t v


  let subRecSize program get_sizebound rec_v v =
    let open VarRec in
    match rec_v with
    | Recursion (loc, var, map) ->
        let transitions_ending_return_locations =
          Set.diff (Program.reachable_locations program loc) (Program.return_locations program)
          |> Set.to_list
          |> List.map ~f:(Program.ending_in_loc program)
          |> List.map ~f:Set.to_list |> List.concat |> TransitionSet.of_list |> Set.to_list
        in
        let lsb_return =
          Bound.max_seq
            (Sequence.of_list @@ List.map ~f:(fun t -> get_sizebound t v) transitions_ending_return_locations)
        in
        let lsb_jump =
          if Set.mem (Program.return_locations program) loc then
            Bound.of_poly @@ Map.find_exn map var
          else
            Bound.zero
        in
        Bound.max lsb_return lsb_jump
    | Var x -> Bound.of_var (Var x)
    | _ -> Bound.infinity


  (** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
      Corresponds to 'SizeBounds for trivial SCCs'. *)
  let compute (program : Program.t) get_sizebound (t, v) lsb_as_bound =
    let open OptionMonad in
    let execute () =
      let res_from_lsb, res_from_update =
        if Program.is_initial program t then
          let res_from_lsb = lsb_as_bound in
          let res_from_update =
            let+ update = TransitionLabel.update (Transition.label t) v in
            if Set.is_subset (PolyRec.vars update) ~of_:(TransitionLabel.input_vars (Transition.label t)) then
              PolyRec.fold ~const:Bound.of_constant ~plus:Bound.add ~times:Bound.mul ~pow:Bound.pow
                ~indeterminate:(fun ind -> subRecSize program get_sizebound ind v)
                update
            else
              Bound.infinity
          in
          (res_from_lsb, res_from_update)
        else
          let res_from_lsb =
            Option.map ~f:(fun lsb -> incoming_bound_lsb program get_sizebound lsb t v) lsb_as_bound
          in
          let res_from_update =
            let+ update = TransitionLabel.update (Transition.label t) v in
            if Set.is_subset (PolyRec.vars update) ~of_:(TransitionLabel.input_vars (Transition.label t)) then
              let lsb =
                PolyRec.fold ~const:Bound.of_constant ~plus:Bound.add ~times:Bound.mul ~pow:Bound.pow
                  ~indeterminate:(fun ind -> subRecSize program get_sizebound ind v)
                  update
              in
              incoming_bound_lifted_update program get_sizebound lsb t v
            else
              Bound.infinity
          in
          (res_from_lsb, res_from_update)
      in
      Bound.(keep_simpler_bound (res_from_lsb |? infinity) (res_from_update |? infinity))
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("compute_trivial_bound", [ ("rv", RV.to_id_string (t, v)) ]))
      ~result:Bound.to_string execute
end
