open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Polynomials
open Formulas


let logger = Logging.(get ExpSize)

module RV = ERVG.RV
module Solver = SMT.IncrementalZ3Solver

let compute_ elcb_cache program get_timebound_gt get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) =
  let incoming_transitions =
    ExpBoundsHelper.entry_transitions logger program (List.map (fst % fst) scc)
  in

  let scc_vars = List.unique ~eq:Var.equal @@ List.map RV.variable scc in

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
      ExpLocalChangeBound.(reduced_elcb elcb_cache ((gt,target_loc),var))
      (* perform appr_substitution with all pre size bounds *)
      |> fun b -> Enum.map (appr_substitution_pre_size b) pre_trans
      |> RealBound.maximum
    in

    var_change_bound
  in

  let loop_effect =
    let execute = fun () ->
      let calc_bound (timebound,sizebound) =
        if RealBound.is_infinity timebound then
          if RealBound.(equal zero sizebound) then
            RealBound.zero
          else
            RealBound.infinity
        else
          if RealBound.is_infinity sizebound then
            if RealBound.(equal zero timebound) then
              RealBound.zero
            else
              RealBound.infinity
          else
            RealBound.(timebound * sizebound)
      in

      scc
      |> List.enum
      |> Enum.map (fun ((gt,l),v) ->
          calc_bound (get_exptimebound gt, result_variable_effect_exp (gt,l) v)
          |> tap (fun eff -> Logger.log logger Logger.DEBUG
              (fun () -> "rv effect",
                [ "rv", RV.to_id_string ((gt,l),v);
                  "tbound", RealBound.to_string (get_exptimebound gt);
                  "sbound", RealBound.to_string (result_variable_effect_exp (gt,l) v);
                  "eff", RealBound.to_string eff ]))
         )
      |> RealBound.sum
    in
    Logger.with_log logger Logger.DEBUG (fun () -> "loop_effect", []) ~result:RealBound.to_string execute
  in

  (** Corresponds to the definition of the starting value in the thesis. *)
  let starting_value =
    incoming_transitions
    |> Enum.map (fun (gt,l) -> Enum.map (fun v -> get_expsizebound (gt,l) v) (List.enum scc_vars))
    |> Enum.map RealBound.maximum
    |> RealBound.sum
    |> tap (fun starting_value -> Logger.log logger Logger.DEBUG
                                             (fun () -> "starting_value", ["result", RealBound.to_string starting_value; "scc_vars", Util.enum_to_string Var.to_string @@ List.enum scc_vars]))
  in

  if time_check then
    RealBound.(starting_value + loop_effect)
    |> RealBound.simplify_vars_nonnegative
  else
    RealBound.infinity

(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let compute elcb_cache program get_timebound_gt get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) =
  let execute () =
    compute_ elcb_cache program get_timebound_gt get_exptimebound get_sizebound get_expsizebound scc
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_nontrivial_bound", ["scc", ERVG.rvs_to_id_string scc])
                     ~result:RealBound.to_string
                     execute
