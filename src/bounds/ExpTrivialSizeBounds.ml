open Batteries
open Polynomials
open BoundsInst
open Formulas
open ProgramTypes

let logger = Logging.(get ExpSize)

type kind = [ `Lower | `Upper ] [@@deriving show]

(* Find an upper bound to the nonprobabilistic size given by get_sizebound of (gt,l) *)
let max_detsizebound elcb_cache ((gt,l),var) get_sizebound =
  GeneralTransition.transitions gt
  |> TransitionSet.filter (Location.equal l % Transition.target)
  |> TransitionSet.enum
  |> Enum.map (fun t -> get_sizebound (t,var) |> RealBound.of_intbound)
  |> RealBound.maximum

(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    Here we add all incomin bounds on absolute values *)
let propagate_bound elcb_cache program get_sizebound_abs get_expsizebound (elcb_without_var: RealBound.t) (elcb_with_var: RealBound.t) (gt,l,var) =
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
        |> List.map (fun gt' -> max_detsizebound elcb_cache ((gt', GeneralTransition.start gt), var) get_sizebound_abs)
        |> List.enum |> RealBound.maximum
      in
      (* Propagate expected values if possible *)
      if ExpLocalChangeBound.bound_is_concave elcb_cache elcb_without_var then
        (* entirely nonprobabalistic propagation *)
        elcb_with_var
        |> RealBound.appr_substition_abs_all prevalues_exp
      else
        (* nonprobabalistically propagate the change *)
        elcb_without_var
        |> RealBound.appr_substition_abs_all prevalues
        (* and add it to the previous expected value *)
        |> RealBound.add (RealBound.appr_substition_abs_all prevalues_exp (RealBound.of_var var))
    in
    let pre_gts = Program.pre_gt program gt in
    pre_gts
    |> substitute_with_prevalues
    |> RealBound.simplify_vars_nonnegative

  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["exp_upd_poly", RealBound.to_string elcb_with_var;
                                                                   "transition", GeneralTransition.to_string gt])
                  ~result:RealBound.to_string
                  execute

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute elcb_cache program get_sizebound (get_expsizebound: (GeneralTransition.t * Location.t) -> Var.t -> RealBound.t) get_timebound_gt ((gt,loc),var) =
  if not (Bound.(get_timebound_gt gt <= Bound.one) = Some true) then
    RealBound.infinity
  else
    let elcb          = ExpLocalChangeBound.(elcb elcb_cache ((gt,loc),var)) in
    let elcb_plus_var = ExpLocalChangeBound.(elcb_plus_var elcb_cache ((gt,loc),var)) in
    let execute () =
      if Program.is_initial_gt program gt then
        elcb_plus_var
      else
        propagate_bound
          elcb_cache
          program
          get_sizebound
          (uncurry get_expsizebound) elcb elcb_plus_var (gt,loc,var)
    in Logger.with_log logger Logger.DEBUG
                         (fun () -> "compute expected trivial bound",
                                    [ "rv", ApproximationModules.ERV.to_id_string ((gt,loc),var)
                                    ; "elcb_plus_var", RealBound.to_string elcb_plus_var])
                         ~result:RealBound.to_string
                         execute
