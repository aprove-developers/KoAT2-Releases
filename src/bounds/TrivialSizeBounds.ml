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
    if LocalSizeBound.is_constant lsb then
      LocalSizeBound.as_bound lsb
    else
      let substitute_with_prevalues t' = LocalSizeBound.as_substituted_bound (fun v -> get_sizebound t' v) lsb in
      let pre_transitions =
        RVG.pre rvg (t,v)
        |> Enum.map RV.transition
        |> Enum.uniq_by Transition.same
      in
      pre_transitions
      |> Enum.map substitute_with_prevalues
      |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_highest_incoming_bound", ["lsb", (Bound.to_string % LocalSizeBound.as_bound) lsb;
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
      LocalSizeBound.(
      Option.map as_bound lsb
      |? Bound.infinity )
    else
      LocalSizeBound.(
      lsb
      |> Option.map (fun lsb -> incoming_bound rvg get_sizebound lsb t v)
      |? Bound.infinity)

  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_trivial_bound", ["rv", RV.to_id_string (t,v)])
                     ~result:Bound.to_string
                     execute
