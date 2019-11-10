open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Polynomials
open Formulas


let logger = Logging.(get ExpSize)

module RV = Make_RV(RVTransitions.TransitionForExpectedSize)
module Solver = SMT.IncrementalZ3Solver

let compute_ program get_timebound_gt get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) v =
  let incoming_transitions =
    ExpBoundsHelper.entry_transitions logger program (List.map (fst % fst) scc)
  in

  let time_check =
    Enum.clone incoming_transitions
    |> Enum.map (get_timebound_gt % fst)
    |> Bound.sum
    |> fun s -> (Bound.(s <= Bound.one) = Some true)
  in

  let result_variable_effect_exp (gt,target_loc) var: RealBound.t =
    let pre_trans =
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal target_loc % Transition.target)
      |> TransitionSet.any
      |> Program.pre program
    in

    let appr_substitution_pre_size bound pret =
      RealBound.appr_substition_abs_all
        (RealBound.of_intbound % get_sizebound pret) bound
    in

    let var_change_bound =
      ExpLocalSizeBound.elsb program ((gt,target_loc),var)
      (* perform appr_substitution with all pre size bounds *)
      |> fun b -> Enum.map (appr_substitution_pre_size b) pre_trans
      |> RealBound.maximum
    in

    var_change_bound
  in

  let loop_effect var =
    let calc_bound (timebound,sizebound) =
      if RealBound.is_infinity timebound then
        if RealBound.(equal zero sizebound) then
          RealBound.zero
        else
          RealBound.infinity
      else
        RealBound.(timebound * sizebound)
    in

    let module TransExpSize = Set.Make2 (GeneralTransition) (Location) in
    scc
    |> List.filter (Var.equal var % snd)
    |> List.map (fun ((gt,l),v) -> (gt,l))
    |> TransExpSize.Product.of_list
    |> TransExpSize.Product.enum
    |> Enum.map (fun (gt,l) -> calc_bound (get_exptimebound gt, result_variable_effect_exp (gt,l) var))
    |> RealBound.sum
  in

  (** Corresponds to the definition of the starting value in the thesis. *)
  let starting_value v =
    incoming_transitions
    |> Enum.map (fun (gt,l) -> get_expsizebound (gt,l) v)
    |> RealBound.sum
    |> tap (fun starting_value -> Logger.log logger Logger.DEBUG
                                             (fun () -> "starting_value", ["result", RealBound.to_string starting_value]))
  in

  if time_check then
    RealBound.(starting_value v + loop_effect v)
  else
    RealBound.infinity

(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let compute program get_timebound_gt get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) v =
  let execute () =
    compute_ program get_timebound_gt get_exptimebound get_sizebound get_expsizebound scc v
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_nontrivial_bound", ["scc", ERVG.rvs_to_id_string scc])
                     ~result:RealBound.to_string
                     execute