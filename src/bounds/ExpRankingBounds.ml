open Batteries
open BoundsInst
open Polynomials
open ProgramTypes
open Formulas

let logger = Logging.(get ExpTime)

(** All entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
let entry_transitions (program: Program.t) (rank_transitions: GeneralTransition.t list): ((GeneralTransition.t * Location.t) Enum.t) =
  let gts =
    Program.generalized_transitions program |> GeneralTransitionSet.to_list
  in
  let single_entry_transitions =
    rank_transitions
    |> List.enum
    |> Enum.map (Program.pre program % TransitionSet.any % GeneralTransition.transitions)
    |> Enum.flatten
    |> Enum.filter (fun r ->
           rank_transitions
           |> List.enum
           |> Enum.map (TransitionSet.enum % GeneralTransition.transitions)
           |> Enum.flatten
           |> Enum.for_all (not % Transition.same r)
         )
    |> Enum.uniq_by Transition.same
  in
  single_entry_transitions
  |> Enum.map (fun transition ->
                 (List.find (TransitionSet.mem transition % GeneralTransition.transitions) gts, Transition.target transition) )
  |> Enum.uniq_by (fun (gt1,l1) (gt2,l2) -> GeneralTransition.same gt1 gt2 && Location.equal l1 l2)
  |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                               (fun () -> "entry_transitions", ["result", transitions
                                          |> Enum.clone |> Util.enum_to_string
                                          (fun (gt,l) -> "(" ^ GeneralTransition.to_id_string gt ^ ", " ^ Location.to_string l ^ ")")]))

let check_if_rank_nonnegative program incoming_list (rank: Location.t -> RealPolynomial.t) =
  let module Solver = SMT.Z3Solver in
  let locs =
    Enum.map (Tuple2.second) incoming_list
    |> LocationSet.of_enum |> LocationSet.enum
  in
  let check_if_rank_geq_0 inv (rank_eval: RealPolynomial.t) =
    let formula = RealFormula.implies (RealFormula.mk inv) RealFormula.Infix.(rank_eval >= RealPolynomial.zero) in
    try RealFormula.neg formula |> Solver.satisfiable |> Bool.neg
    with (Failure _ ) -> false
  in

  Enum.for_all
    (fun l ->
      check_if_rank_geq_0 (Program.invariant l program |> Constraints.RealConstraint.of_intconstraint) (rank l))
    locs

let get_best_bound program incoming_enum appr rankfunc : RealBound.t =
  incoming_enum
  |> Enum.map (fun (gt,l) ->
       let trans_to_l =
         GeneralTransition.transitions gt
         |> TransitionSet.filter (fun (_,t,l') -> Location.equal l' l)
         |> TransitionSet.enum
       in
       let gtrans_to_l =
         Enum.filter (Location.equal l % Tuple2.second) incoming_enum
       in

       let up_of_l var =
         gtrans_to_l
         |> Enum.map (fun (gt',l') -> Approximation.expsizebound `Upper appr (gt',l') var)
         |> RealBound.maximum
       in
       let lo_of_l var =
         gtrans_to_l
         |> Enum.map (fun (gt',l') -> Approximation.expsizebound `Lower appr (gt',l') var)
         |> RealBound.minimum
       in
       let timebound = Enum.map (Approximation.timebound appr) trans_to_l |> Bound.sum |> RealBound.of_intbound in

       let rank = LexRSM.rank rankfunc l in

       let inctime = timebound in
       let rhs = RealBound.( appr_substitution `Upper ~lower:(lo_of_l) ~higher:(up_of_l) (RealBound.of_poly rank)) in

       let mul_inctime_and_rhs (inctime, rhs) = RealBound.(
         if is_infinity inctime then
           if equal zero rhs then
             zero
           else
             infinity
         else
           if is_infinity rhs then
             infinity
           else
             inctime * rhs
       )
       in

       mul_inctime_and_rhs (inctime,rhs)
     )
  |> RealBound.sum

let compute_bound (appr: Approximation.t) (program: Program.t) (rank: LexRSM.t): RealBound.t =
  let execute () =
    rank
    |> LexRSM.non_increasing
    |> GeneralTransitionSet.to_list
    |> entry_transitions program
    |> fun incoming_list ->
        match check_if_rank_nonnegative program incoming_list (LexRSM.rank rank) with
        | true -> get_best_bound program incoming_list appr rank
        | false -> RealBound.infinity
  in Logger.with_log logger Logger.DEBUG
       (fun () -> "compute_bound", ["rank", ""])
                     ~result:RealBound.to_string
                     execute

let add_bound =
  Approximation.add_exptimebound

let improve_with_rank program appr (rank: LexRSM.t) =
  let bound = compute_bound appr program rank in
  if RealBound.is_infinity bound then
    MaybeChanged.same appr
  else
    rank
    |> LexRSM.decreasing
    |> (fun t -> add_bound bound t appr)
    |> MaybeChanged.changed

(** Checks if a transition is bounded *)
let exp_bounded appr transition =
  Approximation.is_exptime_bounded appr transition

let improve program appr =
  program
  |> Program.non_trivial_transitions
  |> GeneralTransitionSet.of_transitionset
  |> GeneralTransitionSet.filter (fun t -> not (exp_bounded appr t))
  |> GeneralTransitionSet.enum
  |> MaybeChanged.fold_enum (fun appr gt ->
         LexRSM.find program gt
         |> Option.map_default (fun rank ->
              improve_with_rank program appr rank
            ) (MaybeChanged.return appr)
       ) appr
