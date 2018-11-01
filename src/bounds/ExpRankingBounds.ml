open Batteries
open BoundsInst
open Polynomials
open ProgramTypes

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

let apply_exp
  (get_expsizebound: [`Lower | `Upper] -> (GeneralTransition.t * Location.t) -> Var.t -> RealBound.t)
  (rank: RealPolynomial.t)
  ((gt,l): GeneralTransition.t * Location.t): RealBound.t =
    rank
    |> RealBound.of_poly
    |> RealBound.appr_substitution
         `Upper
         ~lower:(get_expsizebound `Lower (gt,l))
         ~higher:(get_expsizebound `Upper (gt,l))

let apply
  (get_sizebound: [`Lower | `Upper] -> (Transition.t) -> Var.t -> Bound.t)
  (rank: Polynomial.t)
  (transition: Transition.t): RealBound.t =
    rank
    |> RealPolynomial.of_intpoly
    |> RealBound.of_poly
    |> RealBound.appr_substitution
         `Upper
         ~lower:(RealBound.of_intbound % get_sizebound `Lower transition)
         ~higher:(RealBound.of_intbound % get_sizebound `Upper transition)

let get_best_bound incoming_enum appr rankfunc : RealBound.t =
  incoming_enum
  |> Enum.map (fun (gt,l) ->
       let trans_to_l =
         GeneralTransition.transitions gt
         |> TransitionSet.filter (fun (_,t,l') -> Location.equal l' l)
         |> TransitionSet.enum
       in
       let timebound = Enum.map (Approximation.timebound appr) trans_to_l |> Bound.sum |> RealBound.of_intbound in

       let rank = LexRSM.rank rankfunc l in

       let inctime = timebound in
       let rhs = RealBound.(max zero (apply_exp (fun kind -> Approximation.expsizebound kind appr) rank (gt,l))) in

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
    |> fun incoming_list -> get_best_bound incoming_list appr rank
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
