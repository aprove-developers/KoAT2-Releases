open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes
open Polynomials
open Formulas


(*
 * IMPORTANT NOTE PLEASE READ BEFORE CHANGING
 * Note that this method works only correct when the timebounds are constructed locally.
 * Otherwise we have to check if the program will always enter this scc until the notion
 * of local time bounds is formally introduced and dealt with in this implementation
 *)

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
      (scc: RV.t list)
      (v: Var.t)
      =

  (** Returns all result variables that may influence the given result variable and that are not part of the scc. *)
  let incoming_transitions =
    let transitions_in_scc =
      scc
      |> List.map (fun ((gt,_),_) -> gt)
      |> List.map (TransitionSet.to_list % GeneralTransition.transitions)
      |> List.flatten
    in

    transitions_in_scc
    |> List.map (List.of_enum % Program.pre program)
    |> List.flatten
    |> TransitionSet.of_list
    |> (flip TransitionSet.diff) (TransitionSet.of_list transitions_in_scc)
  in

  let result_variable_effect_exp (gt,target_loc) var: RealBound.t =
    let bound_change_with_0 bound =
      match kind with
      | `Upper -> RealBound.max bound RealBound.zero
      | `Lower -> RealBound.min bound RealBound.zero
    in

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
          (fun pret -> RealBound.appr_substitution kind
             ~lower:(RealBound.of_intbound % get_sizebound `Lower pret)
             ~higher:(RealBound.of_intbound % get_sizebound `Upper pret) bound)
      |> List.enum
      |> match kind with
         | `Upper -> RealBound.maximum
         | `Lower -> RealBound.minimum
    in

    let var_change_bound =
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal target_loc % Transition.target)
      |> TransitionSet.to_list
      |> List.map (fun t -> ExpLocalSizeBound.det_update kind (t,var))
      |> Util.option_sequence
      |> Option.map (List.map (RealPolynomial.of_var var |> flip RealPolynomial.sub))
      |> Option.map (List.map (GeneralTransition.guard gt |> ExpLocalSizeBound.simplify_poly_with_guard))
      |> Option.map (List.map RealBound.of_poly)
      (* innermost max/min against 0 *)
      |> Option.map (List.map bound_change_with_0)
      (* perform appr_substitution with all pre size bounds *)
      |> Option.map (List.map appr_substitution_pre_size)
      |> Option.map List.enum
      |> Option.map (fun en -> match kind with
                       | `Upper -> RealBound.maximum en
                       | `Lower -> RealBound.minimum en )
      |? match kind with
         | `Lower -> RealBound.minus_infinity
         | `Upper -> RealBound.infinity
    in

    var_change_bound
  in

  let loop_effect var =
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

    let module TransExpSize = Set.Make2 (GeneralTransition) (Location) in
    scc
    |> List.map (fun ((gt,l),v) -> (gt,l))
    |> TransExpSize.Product.of_list
    |> TransExpSize.Product.to_list
    |> List.map (fun (gt,l) -> calc_bound (get_exptimebound gt, result_variable_effect_exp (gt,l) var))
    |> List.enum
    |> RealBound.sum
  in

  (** Corresponds to the definition of the starting value in the thesis. *)
  let starting_value v =
    incoming_transitions
    |> TransitionSet.to_list
    |> List.map (fun t -> get_sizebound kind t v)
    |> List.enum
    |> Bound.maximum
    |> RealBound.of_intbound
    |> tap (fun starting_value -> Logger.log logger Logger.DEBUG
                                             (fun () -> "starting_value", ["result", RealBound.to_string starting_value]))
  in

  RealBound.(starting_value v + loop_effect v)

(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let compute kind program rvg get_timebound get_exptimebound get_sizebound get_expsizebound (scc: RV.t list) v =
  let execute () =
    compute_ kind program rvg (RealBound.of_intbound % get_timebound) get_exptimebound get_sizebound get_expsizebound scc v
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute_nontrivial_bound", ["kind", show_kind kind;
                                                             "scc", ERVG.rvs_to_id_string scc])
                     ~result:RealBound.to_string
                     execute
