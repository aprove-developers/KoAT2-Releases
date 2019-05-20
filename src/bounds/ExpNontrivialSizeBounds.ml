open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Polynomials
open Formulas


let logger = Logging.(get ExpSize)

module RV = Make_RV(RVTransitions.TransitionForExpectedSize)
module Solver = SMT.IncrementalZ3Solver

let compute_ program get_timebound get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) v =

  let incoming_transitions =
    ExpBoundsHelper.entry_transitions logger program (List.map (fst % fst) scc)
  in

  let result_variable_effect_exp (gt,target_loc) var: RealBound.t =
    let appr_substitution_pre_size bound =
      let pre_trans =
        GeneralTransition.transitions gt
        |> TransitionSet.filter (Location.equal target_loc % Transition.target)
        |> TransitionSet.any
        |> List.singleton
        |> List.map (List.of_enum % Program.pre program)
        |> List.flatten
      in
      pre_trans
      |> List.map
          (fun pret -> RealBound.appr_substition_abs_all
             (RealBound.of_intbound % get_sizebound pret) bound)
      |> List.enum
      |> RealBound.maximum

    in

    let var_change_bound =
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal target_loc % Transition.target)
      |> TransitionSet.to_list
      |> List.map (fun t -> LocalSizeBound.sizebound_local_abs_bound program t var)
      |> Util.option_sequence
      |> Option.map
          (fun l ->
            List.map (RealBound.of_var var |> flip RealBound.sub) l
            (* innermost max/min against 0 *)
            |> (List.map RealBound.abs)
            (* perform appr_substitution with all pre size bounds *)
            |> (List.map appr_substitution_pre_size)
            |> List.enum
            |> RealBound.maximum
          )
      |? RealBound.infinity
    in

    var_change_bound
  in

  let loop_effect var =
    let calc_bound (timebound,sizebound) =
      if RealBound.is_infinity timebound then
        if RealBound.(equal zero sizebound) then
          RealBound.zero
        else
          RealBound.(infinity * sizebound)
      else
        RealBound.(timebound * sizebound)
    in

    let module TransExpSize = Set.Make2 (GeneralTransition) (Location) in
    scc
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

  RealBound.(starting_value v + loop_effect v)

(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let compute program get_timebound get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) v =
  let execute () =
    compute_ program (RealBound.of_intbound % get_timebound) get_exptimebound get_sizebound get_expsizebound scc v
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_nontrivial_bound", ["scc", ERVG.rvs_to_id_string scc])
                     ~result:RealBound.to_string
                     execute