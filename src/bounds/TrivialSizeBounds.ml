open Batteries
open BoundsInst
open ProgramTypes

let logger = Logging.(get Size)

module RV = RVGTypes.RVG.RV

type kind = [ `Lower | `Upper ] [@@deriving show]

(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    S'(alpha) = max(S_l(alpha)(S(t',v_1),...,S(t',v_n)) for all t' in pre(t)) *)
let incoming_bound pre_cache kind program get_sizebound lsb t =
  let execute () =
    let substitute_with_prevalues t' = LocalSizeBound.as_substituted_bound (fun kind v -> get_sizebound kind t' v) lsb in
    t
    |> Program.pre pre_cache program
    |> List.enum
    |> Enum.map substitute_with_prevalues
    |> match kind with
        | `Lower -> Bound.minimum
        | `Upper -> Bound.maximum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["lsb", (Bound.to_string % LocalSizeBound.as_bound) lsb;
                                                                   "transition", Transition.to_id_string t])
                  ~result:Bound.to_string
                  execute

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute cache kind program get_sizebound (t,v) =
  let execute () =
    let (lsb_cache, pre_cache) = (CacheManager.lsb_cache cache, CacheManager.pre_cache cache) in
    let (lsb: LocalSizeBound.t Option.t) =
      LocalSizeBound.sizebound_local_rv lsb_cache program kind (t, v)
    in
    if Program.is_initial program t then
      LocalSizeBound.(
      lsb
      |> Option.map as_bound
      |? default kind)
    else
      LocalSizeBound.(
      lsb
      |> Option.map (fun lsb -> incoming_bound pre_cache kind program get_sizebound lsb t)
      |? default kind)

  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute trivial bound", ["kind", show_kind kind;
                                                          "rv", RV.to_id_string (t,v)])
                     ~result:Bound.to_string
                     execute

