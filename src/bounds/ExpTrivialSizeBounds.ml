open Batteries
open Polynomials
open BoundsInst
open Formulas
open ProgramTypes

let logger = Logging.(get ExpSize)

type kind = [ `Lower | `Upper ] [@@deriving show]

let max_detsizebound elsb_cache ((gt,l),var) get_sizebound =
  GeneralTransition.transitions gt
  |> TransitionSet.filter (Location.equal l % Transition.target)
  |> TransitionSet.to_list
  |> List.map (fun t -> get_sizebound (t,var) |> RealBound.of_intbound)
  |> List.enum
  |> RealBound.maximum

(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    Here we add all incomin bounds on absolute values *)
let incoming_bound elsb_cache program get_sizebound_abs get_expsizebound (elsb_with_prob: RealBound.t) (gt,l) =
  let execute () =
    let substitute_with_prevalues gtset =
      let prevalues_exp var =
        GeneralTransitionSet.to_list gtset
        |> List.map
            (fun gt' -> get_expsizebound ((gt',GeneralTransition.start gt),var))
        |> List.enum
        |> RealBound.sum
      in
      let prevalues var =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> max_detsizebound elsb_cache ((gt', GeneralTransition.start gt), var) get_sizebound_abs)
        |> List.enum |> RealBound.maximum
      in
      (* Propagate expected values if possible *)
      if ExpLocalSizeBound.bound_is_concave elsb_cache elsb_with_prob then
        elsb_with_prob
        |> RealBound.appr_substition_abs_all (prevalues_exp)
      else
        elsb_with_prob
        |> RealBound.appr_substition_abs_all prevalues
    in
    let pre_gts = Program.pre_gt program gt in
    pre_gts
    |> substitute_with_prevalues

  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["exp_upd_poly", RealBound.to_string elsb_with_prob;
                                                                   "transition", GeneralTransition.to_string gt])
                  ~result:RealBound.to_string
                  execute

module ERV = ApproximationModules.ERV

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute elsb_cache program get_sizebound (get_expsizebound: (GeneralTransition.t * Location.t) -> Var.t -> RealBound.t) get_timebound_gt ((gt,loc),var) =
  if not (Bound.(get_timebound_gt gt <= Bound.one) = Some true) then
    RealBound.infinity
  else
    let elsb          = ExpLocalSizeBound.elsb elsb_cache program ((gt,loc),var) in
    let elsb_plus_var = RealBound.(elsb + abs (of_var var)) in
    let execute () =
      if Program.is_initial_gt program gt then
        elsb_plus_var
      else
        incoming_bound
          elsb_cache
          program
          get_sizebound
          (uncurry get_expsizebound) elsb_plus_var (gt,loc)
    in Logger.with_log logger Logger.DEBUG
                         (fun () -> "compute expected trivial bound",
                                    [ "rv", ERV.to_id_string ((gt,loc),var)
                                    ; "elsb_plus_var", RealBound.to_string elsb_plus_var])
                         ~result:RealBound.to_string
                         execute
