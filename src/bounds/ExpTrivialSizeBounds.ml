open Batteries
open Polynomials
open BoundsInst
open Formulas
open ProgramTypes

let logger = Logging.(get ExpSize)

type kind = [ `Lower | `Upper ] [@@deriving show]

let max_detsizebound ((gt,l),var) get_sizebound =
  GeneralTransition.transitions gt
  |> TransitionSet.filter (Location.equal l % Transition.target)
  |> TransitionSet.to_list
  |> List.map (fun t -> get_sizebound (t,var) |> RealBound.of_intbound)
  |> List.enum
  |> RealBound.maximum

let formula_implied_by_formula formula1 formula2 =
  formula2
  (* Does formula1 always imply formula2 ? *)
  |> Formula.implies formula1
  |> Formula.neg
  (* Try to find a contra *)
  |> SMT.Z3Opt.unsatisfiable

let only_one_gt_outgoing program gt =
  let start_loc = GeneralTransition.start gt in
  let gts = Program.generalized_transitions program in
  gts
  |> GeneralTransitionSet.filter (Location.equal start_loc % GeneralTransition.start)
  |> fun gtset -> GeneralTransitionSet.for_all
       ( fun gt ->
          let gtset_without_gt = GeneralTransitionSet.remove gt gtset in
          let rhs              =
            GeneralTransitionSet.fold
              (fun gtselem f -> Formula.mk_or f (GeneralTransition.guard gtselem |> Formula.mk))
              gtset_without_gt Formula.mk_false
          in
          formula_implied_by_formula (GeneralTransition.guard gt |> Formula.mk) (Formula.neg rhs)
       ) gtset

(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    Here we add all incomin bounds on absolute values *)
let incoming_bound program get_sizebound_abs get_expsizebound (elsb_with_prob: RealBound.t) (gt,l) =
  let execute () =
    let substitute_with_prevalues gtset =
      let prevalues_exp var =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> get_expsizebound ((gt',GeneralTransition.start gt),var))
        |> List.enum
        |> RealBound.sum
      in
      let prevalues var =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> max_detsizebound ((gt', GeneralTransition.start gt), var) get_sizebound_abs)
        |> List.enum |> RealBound.maximum
      in
      elsb_with_prob
      |> RealBound.appr_substitution_probabilistic_and_nonprobabilistic
          (* Propagate expected values if possible *)
          ~probabilistic:(prevalues_exp)
          ~nonprobabilistic:(prevalues)
    in
    let pre_gts = Program.pre_gt program gt in
    pre_gts
    |> substitute_with_prevalues

  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["exp_upd_poly", RealBound.to_string elsb_with_prob;
                                                                   "transition", GeneralTransition.to_string gt])
                  ~result:RealBound.to_string
                  execute

let only_one_gt_going_to_loc orig_graph target_loc sub =
  let locations = TransitionGraph.locations sub in
  let transitions =
    TransitionGraph.generalized_transitions orig_graph
    |> GeneralTransitionSet.filter (fun gt -> LocationSet.mem (GeneralTransition.start gt) locations &&
                                              LocationSet.exists (Location.equal target_loc) (GeneralTransition.targets gt))
  in
  GeneralTransitionSet.for_all ((=) 1 % LocationSet.cardinal % GeneralTransition.targets) transitions &&
  GeneralTransitionSet.cardinal transitions = 1

module ERV = ApproximationModules.ERV

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute program get_sizebound (get_expsizebound: (GeneralTransition.t * Location.t) -> Var.t -> RealBound.t) get_timebound ((gt,loc),var) =
  if not (only_one_gt_outgoing program gt) then
    RealBound.infinity
  else
    let elsb = ExpLocalSizeBound.elsb program ((gt,loc),var) in

    let execute () =
      let probability_of_rv =
        GeneralTransition.transitions gt
        |> TransitionSet.filter (Location.equal loc % Transition.target)
        |> TransitionSet.total_probability
      in
      let elsb_with_prob = RealBound.(elsb * (of_constant probability_of_rv)) in
      if Program.is_initial_gt program gt then
        elsb_with_prob
      else
        incoming_bound
          program
          get_sizebound
          (uncurry get_expsizebound) elsb_with_prob (gt,loc)
    in Logger.with_log logger Logger.DEBUG
                         (fun () -> "compute expected trivial bound",
                                    [ "rv", ERV.to_id_string ((gt,loc),var)
                                    ; "elsb", RealBound.to_string elsb])
                         ~result:RealBound.to_string
                         execute