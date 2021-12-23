(** Modules used to infer size-bounds for trivial components. *)
open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes

(** Modules used to infer size-bounds for trivial components. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'.*)

(** Logger Size *)
let logger = Logging.(get Size)

(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    S'(alpha) = max(S_l(alpha)(S(t',v_1),...,S(t',v_n)) for all t' in pre(t)). *)
let incoming_bound rvg get_sizebound lsb t v =
  (* since this is a trivial scc*)
  let execute () =
    (* If the LSB is constant there are no pre-transitions in the RVG *)
    if Bound.is_constant lsb then
      lsb
    else
      let substitute_with_prevalues t' = Bound.substitute_f (fun v -> get_sizebound t' v) lsb in
      let pre_transitions =
        RVG.pre rvg (t,v)
        |> Enum.map RV.transition
        |> Enum.uniq_by Transition.same
      in
      pre_transitions
      |> Enum.map substitute_with_prevalues
      |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_highest_incoming_bound", ["lsb", Bound.to_string lsb;
                                                                   "transition", Transition.to_id_string t])
                  ~result:Bound.to_string
                  execute

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute program rvg get_sizebound (t,v) =
  let execute () =
    let (lsb: LocalSizeBound.t Option.t) =
      LocalSizeBound.sizebound_local_rv program (t, v)
    in
    if Program.is_initial program t then
      match lsb with
        | Some b -> LocalSizeBound.as_bound b
        | None ->
            let tlabel = Transition.label t in
            match TransitionLabel.update tlabel v with
              | Some u ->
                  if VarSet.subset (Polynomials.Polynomial.vars u) (TransitionLabel.input_vars tlabel) then
                    Bound.of_poly u
                  else Bound.infinity
              | None   -> Bound.infinity
    else
      match lsb with
        | Some b -> incoming_bound rvg get_sizebound (LocalSizeBound.as_bound b) t v
        | None ->
            let tlabel = Transition.label t in
            match TransitionLabel.update tlabel v with
              | Some u ->
                  if VarSet.subset (Polynomials.Polynomial.vars u) (TransitionLabel.input_vars tlabel) then
                    Bound.of_poly u
                  else Bound.infinity
              | None   -> Bound.infinity
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_trivial_bound", ["rv", RV.to_id_string (t,v)])
                     ~result:Bound.to_string
                     execute
