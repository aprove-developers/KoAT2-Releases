open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Polynomials
open Formulas

let logger = Logging.(get ExpSize)

module RV = Make_RV(RVTransitions.TransitionForExpectedSize)
module Solver = SMT.IncrementalZ3Solver

type kind = [ `Lower | `Upper ] [@@deriving show]

let sign = function
  | `Lower -> Bound.neg
  | `Upper -> identity

let compute_
      (kind: kind)
      (program: Program.t)
      (rvg: ERVG.t)
      (get_timebound: Transition.t -> RealBound.t)
      (get_exptimebound: GeneralTransition.t -> RealBound.t)
      (get_sizebound: kind -> Transition.t -> Var.t -> Bound.t)
      (get_expsizebound: kind -> (GeneralTransition.t * Location.t) -> Var.t -> RealBound.t)
      (scc: RV.t list) =

  (** Returns all result variables that may influence the given result variable and that are not part of the scc. *)
  let pre_out_scc rv =
    rv
    |> ERVG.pre rvg
    |> Util.without RV.same (List.enum scc)
  in

  let result_variable_effect_exp kind rv: RealBound.t =
    let gt = RV.transition rv |> RVTransitions.TransitionForExpectedSize.gt in
    let target_loc = RV.transition rv |> RVTransitions.TransitionForExpectedSize.loc in
    let var = RV.variable rv in

    let var_bounds sign v =
      let bounds =
        ERVG.pre rvg rv
        |> Enum.map (fun ((gt,l),v) ->
            GeneralTransition.transitions gt
            |> TransitionSet.filter (Location.equal l % Transition.target) |> TransitionSet.enum)
        |> Enum.flatten
        |> Enum.map (fun t -> get_sizebound kind t v |> RealBound.of_intbound)
      in
      match sign with
      | `Upper -> RealBound.maximum bounds |> RealBound.(max zero)
      | `Lower -> RealBound.minimum bounds |> RealBound.(min zero)
    in

    let var_change_bound =
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal target_loc % Transition.target)
      |> TransitionSet.to_list
      |> List.map (fun t -> ExpLocalSizeBound.det_update kind (t,RV.variable rv))
      |> Util.option_sequence
      |> Option.map (List.map (RealPolynomial.of_var var |> flip RealPolynomial.sub))
      |> Option.map (List.map (GeneralTransition.guard gt |> ExpLocalSizeBound.simplify_poly_with_guard))
      |> Option.map (List.map RealBound.of_poly)
      |> Option.map List.enum
      |> Option.map (fun en -> match kind with
                       | `Upper -> RealBound.maximum en
                       | `Lower -> RealBound.minimum en )
      |> Option.map
          (RealBound.appr_substitution kind ~lower:(var_bounds `Lower) ~higher:(var_bounds `Upper))
      |? match kind with
         | `Lower -> RealBound.minus_infinity
         | `Upper -> RealBound.infinity
    in

    let exp_gt_timebound =
      get_exptimebound gt
    in
    let prob_gtl_of_gt =
      let targetl = RV.transition rv |> RVTransitions.TransitionForExpectedSize.loc in
      RV.transition rv
      |> RVTransitions.TransitionForExpectedSize.gt
      |> GeneralTransition.transitions
      |> TransitionSet.filter (Location.equal targetl % Transition.target)
      |> fun tset -> TransitionSet.fold OurFloat.(fun t p -> (TransitionLabel.probability (Transition.label t)) + p) tset (OurFloat.zero)
    in

    let calc_bound (timebound,sizebound) =
      let sizebound_min_max =
        match kind with
        | `Upper -> RealBound.max RealBound.zero sizebound
        | `Lower -> RealBound.min RealBound.zero sizebound
      in

      if RealBound.is_infinity timebound then
        if RealBound.(equal zero sizebound_min_max) then
          RealBound.zero
        else
          RealBound.(infinity * sizebound_min_max)
      else
        RealBound.(timebound * sizebound_min_max)
    in

    let timebound1 = RealBound.(exp_gt_timebound * of_poly (RealPolynomial.of_constant prob_gtl_of_gt)) in

    let sizebound1 = var_change_bound in

    let bounds = [timebound1, sizebound1] |> List.map calc_bound |> List.enum in

    match kind with
    | `Upper -> RealBound.minimum bounds
    | `Lower -> RealBound.maximum bounds
  in

  let loop_effect =
    scc
    |> List.map (result_variable_effect_exp kind)
    |> List.enum
    |> RealBound.sum
  in

  (** Corresponds to the definition of the starting value in the thesis. *)
  let starting_value_exp =
    scc
    |> List.enum
    |> Enum.map pre_out_scc
    |> Enum.flatten
    |> Enum.map (uncurry (get_expsizebound kind))
    |> (match kind with
       | `Upper -> RealBound.maximum
       | `Lower -> RealBound.minimum)
    |> tap (fun starting_value -> Logger.log logger Logger.DEBUG
                                             (fun () -> "starting_value", ["result", RealBound.to_string starting_value]))
  in

  loop_effect
  |> fun loop_effect ->
       RealBound.(starting_value_exp + loop_effect)

(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let compute kind program rvg get_timebound get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) =
  let execute () =
    compute_ kind program rvg (RealBound.of_intbound % get_timebound) get_exptimebound get_sizebound get_expsizebound scc
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_nontrivial_bound", ["kind", show_kind kind;
                                                             "scc", ERVG.rvs_to_id_string scc])
                     ~result:RealBound.to_string
                     execute
