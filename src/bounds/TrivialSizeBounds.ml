(** Modules used to infer size-bounds for trivial components. *)
open OurBase
open BoundsInst

(** Modules used to infer size-bounds for trivial components. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'.*)

(** Logger Size *)
let logger = Logging.(get Size)

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module RV = RVGTypes.MakeRV(PM.TransitionLabel)(PM.Transition)
  module RVG = RVGTypes.MakeRVG(PM)

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
        pre_transitions
        |> Base.Sequence.map ~f:substitute_with_prevalues
        |> Bound.sum_sequence
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "compute_highest_incoming_bound", ["lsb", Bound.to_string lsb;
                                                                     "transition", Transition.to_id_string t])
                    ~result:Bound.to_string
                    execute

  let incoming_bound_lsb rvg get_sizebound lsb t v =
    let pre_transitions =
      RVG.pre rvg (t,v)
      |> List.map ~f:RV.transition
      |> TransitionSet.stable_dedup_list
      |> Base.Sequence.of_list
    in
    incoming_bound pre_transitions get_sizebound lsb t v

  let incoming_bound_lifted_update program get_sizebound upd t v =
    let pre_transitions = Program.pre program t in
    incoming_bound pre_transitions get_sizebound (Bound.of_poly upd) t v

  (** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
      Corresponds to 'SizeBounds for trivial SCCs'. *)
  let compute (program: Program.t) rvg get_sizebound (t,v) lsb_as_bound =
    let execute () =
      if Program.is_initial program t then
        match lsb_as_bound with
          | Some b -> b
          | None ->
              let tlabel = Transition.label t in
              match TransitionLabel.update tlabel v with
                | Some u ->
                    if Set.is_subset (Polynomials.Polynomial.vars u) ~of_:(TransitionLabel.input_vars tlabel) then
                      Bound.of_poly u
                    else Bound.infinity
                | None   -> Bound.infinity
      else
        match lsb_as_bound with
          | Some b -> incoming_bound_lsb rvg get_sizebound b t v
          | None ->
              let tlabel = Transition.label t in
              match TransitionLabel.update tlabel v with
                | Some u ->
                    if Set.is_subset (Polynomials.Polynomial.vars u) ~of_:(TransitionLabel.input_vars tlabel) then
                      incoming_bound_lifted_update program get_sizebound u t v
                    else Bound.infinity
                | None   -> Bound.infinity
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "compute_trivial_bound", ["rv", RV.to_id_string (t,v)])
                       ~result:Bound.to_string
                       execute
end
