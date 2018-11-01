open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Polynomials
open Formulas

let logger = Logging.(get ExpNontrivialSize)

module RV = Make_RV(RVTransitions.TransitionForExpectedSize)
module Solver = SMT.IncrementalZ3Solver

type kind = [ `Lower | `Upper ] [@@deriving show]
type sign = [ `Pos | `Neg ] [@@deriving show]
type boundtype = [`ConstChange | `SetToConst]

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

  (** Returns all result variables that may influence the given result variable and that are part of the scc. *)
  let pre_in_scc rv =
    rv
    |> ERVG.pre rvg
    |> Util.intersection RV.same (List.enum scc)
  in

  (** Returns all result variables that may influence the given result variable and that are not part of the scc. *)
  let pre_out_scc rv =
    rv
    |> ERVG.pre rvg
    |> Util.without RV.same (List.enum scc)
  in

  (** Returns all result variables that may influence the given result variable from within the scc.
      Corresponds to V_rv in the thesis. *)
  let scc_variables rv =
    rv
    |> pre_in_scc
    |> Enum.map (fun (t,v) -> v)
    |> Enum.uniq_by Var.equal
  in

  let result_variable_effect_exp rv: RealBound.t =
    let gt = RV.transition rv |> RVTransitions.TransitionForExpectedSize.gt in
    let target_loc = RV.transition rv |> RVTransitions.TransitionForExpectedSize.loc in

    let polynomial_exp_var_change: RealPolynomial.t option =
      let update_exp_poly = ExpLocalSizeBound.exp_poly rv in
      let update_scc_vars = 
        RealPolynomial.simplify update_exp_poly 
        |> RealPolynomial.vars
        |> VarSet.inter (scc_variables rv |> VarSet.of_enum) 
      in
      if VarSet.cardinal update_scc_vars >= 1 then 
        RealPolynomial.sub update_exp_poly (VarSet.any update_scc_vars |> RealPolynomial.of_var)
        |> ExpLocalSizeBound.simplify_poly_with_guard gt
        |> tap (Printf.printf "exp_poly: %s\n" % RealPolynomial.to_string)
        |> fun x -> Some x
      else
        None
    in

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

    let exp_var_bounds sign v =
      let bounds =
        ERVG.pre rvg rv
        |> Enum.map (fun ((gt,l),var) -> get_expsizebound kind (gt,l) var)
      in
      match sign with
      | `Upper -> RealBound.maximum bounds |> RealBound.(max zero)
      | `Lower -> RealBound.minimum bounds |> RealBound.(min zero)
    in

    let var_change_bound = 
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal target_loc % Transition.target)
      |> TransitionSet.to_list
      |> List.map (fun t -> ExpLocalSizeBound.det_update kind gt (t,RV.variable rv))
      |> List.map (Option.map RealBound.of_poly)
      |> Util.option_sequence
      |> Option.map List.enum
      |> Option.map (fun en -> match kind with
                       | `Upper -> RealBound.maximum en
                       | `Lower -> RealBound.maximum en )
      |> Option.map
          (RealBound.appr_substitution kind ~lower:(var_bounds `Lower) ~higher:(var_bounds `Upper))
      |? match kind with
         | `Lower -> RealBound.minus_infinity
         | `Upper -> RealBound.infinity
    in

    let exp_var_change_bound =
      Option.map 
        (fun poly ->
           if ExpLocalSizeBound.appr_substitution_is_valid kind gt poly then
             poly
             |> ExpLocalSizeBound.simplify_poly_with_guard gt
             |> RealBound.of_poly
             |> RealBound.appr_substitution kind ~lower:(exp_var_bounds `Lower) ~higher:(exp_var_bounds `Upper)
           else 
             poly
             |> ExpLocalSizeBound.simplify_poly_with_guard gt
             |> RealBound.of_poly
             |> RealBound.appr_substitution kind ~lower:(exp_var_bounds `Lower) ~higher:(var_bounds `Upper)
      )
      polynomial_exp_var_change
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
      if RealBound.is_infinity timebound then
        if RealBound.(equal zero sizebound) then
          RealBound.zero
        else
          RealBound.infinity
      else
        RealBound.(timebound * sizebound) 
    in

    let timebound1 = RealBound.(exp_gt_timebound * of_poly (RealPolynomial.of_constant prob_gtl_of_gt)) in
    let timebound2 = 
      GeneralTransition.transitions gt |> TransitionSet.filter (Location.equal target_loc % Transition.target)
      |> fun tset -> TransitionSet.fold (fun t -> RealBound.(+) (get_timebound t)) tset RealBound.zero
    in

    let sizebound1 = var_change_bound in
    let sizebound2 = exp_var_change_bound in

    Printf.printf "timebound1: %s  sizebound1: %s\n" (RealBound.to_string timebound1) (RealBound.to_string sizebound1);
    Printf.printf "timebound2: %s  sizebound2: %s\n" (RealBound.to_string timebound2) (RealBound.to_string sizebound2);

    let bounds = [timebound1, sizebound1; timebound2, sizebound2] |> List.map (calc_bound) |> List.enum in
    
    match kind with
    | `Upper -> RealBound.minimum bounds
    | `Lower -> RealBound.maximum bounds
  in

  let loop_effect = 
    scc
    |> List.map result_variable_effect_exp
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
  |> tap (fun _ -> Printf.printf "starting_value: %s\n" (RealBound.to_string starting_value_exp))
  |> tap (fun _ -> Printf.printf "loop_effect %s\n" (RealBound.to_string loop_effect))
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
