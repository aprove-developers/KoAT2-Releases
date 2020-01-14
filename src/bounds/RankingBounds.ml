open Batteries
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas
open CFR

let logger = Logging.(get Time)

let logger_cfr = Logging.(get CFR)

type measure = [ `Cost | `Time ] [@@deriving show, eq]

(** All entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
let entry_transitions (program: Program.t) (rank_transitions: Transition.t list): Transition.t List.t =
  rank_transitions
  |> List.enum
  |> Enum.map (Program.pre program)
  |> Enum.flatten
  |> Enum.filter (fun r ->
         rank_transitions
         |> List.enum
         |> Enum.for_all (not % Transition.same r)
       )
  |> Enum.uniq_by Transition.same
  |> List.of_enum
  |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                               (fun () -> "entry_transitions", ["result", transitions |> List.enum |> Util.enum_to_string Transition.to_id_string]))

let apply (get_sizebound: [`Lower | `Upper] -> Transition.t -> Var.t -> Bound.t) (rank: Polynomial.t) (transition: Transition.t): Bound.t =
  rank
  |> Bound.of_poly
  |> Bound.appr_substitution
       `Upper
       ~lower:(get_sizebound `Lower transition)
       ~higher:(get_sizebound `Upper transition)


(* method transforms polynome to parapolynom*)
let as_parapoly label var =
    match TransitionLabel.update label var with
    (** Correct? In the nondeterministic case we just make it deterministic? *)
    | None -> ParameterPolynomial.of_var var
    | Some p -> ParameterPolynomial.of_polynomial p


(* Computes new bounds*)
let compute_bound_mrf (appr: Approximation.t) (program: Program.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> entry_transitions program
   |> List.enum
   |> Enum.map (fun (l,t,l') ->
       let timebound = Approximation.timebound appr (l,t,l') in 
       let evaluate = (fun rank -> (apply (fun kind -> Approximation.sizebound kind appr) rank) (l,t,l')) in
       let evaluated_rankingFunctions = (List.init (MultiphaseRankingFunction.depth rank) (fun i -> (evaluate ((List.nth (MultiphaseRankingFunction.rank rank) i) l')))) in
       let rhs = if MultiphaseRankingFunction.depth rank = 1 then
          Bound. (max Bound.zero (add Bound.one (List.nth evaluated_rankingFunctions 0)))
          else 
          Bound. (add Bound.one (mul (of_int (MRF_Coefficient.coefficient rank))  (MRF_Coefficient.maxBound_of_list evaluated_rankingFunctions))) in 
        Bound.(
          if is_infinity timebound then
            if equal zero rhs then
              zero
            else
              infinity
          else
            if is_infinity rhs then
              infinity
            else
              timebound * rhs
        ))
   |> Bound.sum    
 in 
    let bound = execute () in
    if Bound.is_linear bound then 
      CFR.nonLinearTransitions := TransitionSet.remove (MultiphaseRankingFunction.decreasing rank) !CFR.nonLinearTransitions
    else
      CFR.nonLinearTransitions := TransitionSet.add (MultiphaseRankingFunction.decreasing rank) !CFR.nonLinearTransitions;
    Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (MultiphaseRankingFunction.decreasing rank);
                                   "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (MultiphaseRankingFunction.non_increasing rank));
                                   "rank", MultiphaseRankingFunction.only_rank_to_string rank;])
                    ~result:Bound.to_string (fun () -> bound)

 let compute_bound (appr: Approximation.t) (program: Program.t) (rank: RankingFunction.t): Bound.t =
   let execute () =
     rank
     |> RankingFunction.non_increasing
     |> entry_transitions program
     |> List.enum
     |> Enum.map (fun (l,t,l') ->
            let timebound = Approximation.timebound appr (l,t,l') in
            let rhs = Bound.(max zero (apply (fun kind -> Approximation.sizebound kind appr) (RankingFunction.rank rank l') (l,t,l'))) in
            Bound.(
              if is_infinity timebound then
                if equal zero rhs then
                  zero
                else
                  infinity
              else
                if is_infinity rhs then
                  infinity
                else
                  timebound * rhs
            ))
     |> Bound.sum
   in 
    let bound = execute () in
    if Bound.is_linear bound then 
      CFR.nonLinearTransitions := TransitionSet.remove (RankingFunction.decreasing rank) !CFR.nonLinearTransitions
    else
      CFR.nonLinearTransitions := TransitionSet.add (RankingFunction.decreasing rank) !CFR.nonLinearTransitions;
   Logger.with_log logger Logger.DEBUG
        (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (RankingFunction.decreasing rank);
                                     "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (RankingFunction.non_increasing rank));
                                     "rank", RankingFunction.only_rank_to_string rank])
                      ~result:Bound.to_string (fun () -> bound)

let add_bound = function
  | `Time -> Approximation.add_timebound
  | `Cost -> Approximation.add_costbound

let improve_with_rank  measure program appr rank =
  let bound = compute_bound appr program rank in
  if Bound.is_infinity bound then
    MaybeChanged.same appr
  else
    rank
    |> RankingFunction.decreasing
    |> (fun t -> add_bound measure bound t appr)
    |> MaybeChanged.changed

  let improve_with_rank_mrf measure program appr rank =
    let bound = compute_bound_mrf appr program rank in
    if Bound.is_infinity bound then
      MaybeChanged.same appr
    else
      rank
      |> MultiphaseRankingFunction.decreasing
      |> (fun t -> add_bound measure bound t appr)
      |> MaybeChanged.changed

(** Checks if a transition is bounded *)
let bounded measure appr transition =
  match measure with
  | `Time -> Approximation.is_time_bounded appr transition
  | `Cost -> false

let improve_scc ?(mrf = false) ?(cfr = false) (scc: TransitionSet.t)  measure program appr =
  let execute () =
    scc
    |> TransitionSet.filter (fun t -> not (bounded measure appr t))
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum (
      if mrf then
      (fun appr transition ->
           MultiphaseRankingFunction.find  measure program transition
           |> List.enum
           |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mrf measure program appr rank
             ) appr)
      else
      (fun appr transition ->
           RankingFunction.find measure program transition
           |> List.enum
           |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank
             ) appr)
         ) appr
      in if cfr && not (TransitionSet.is_empty !CFR.nonLinearTransitions) then (
        ignore(Logger.with_log logger Logger.INFO
            (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc ;"measure", show_measure measure])
            execute);
            ignore (CFR.apply_cfr program);
            nonLinearTransitions := TransitionSet.empty;
            Logger.with_log logger Logger.INFO
              (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
              execute)
        else (Logger.with_log logger Logger.INFO
            (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
            execute)

(* Stops if something has changed (and we compute new sizebounds) Not Working ??? TODO *)
let rec fold_until (f: Approximation.t MaybeChanged.t -> ProgramTypes.TransitionSet.t -> Approximation.t MaybeChanged.t)
                   (p: Approximation.t MaybeChanged.t -> bool) pre = function
    | x :: xs when p pre -> pre
    | x :: xs -> fold_until f p (f pre x) xs
    | [] -> pre

let improve ?(mrf = false) ?(cfr = false) measure program appr  =
Logger.log logger_cfr Logger.INFO (fun () -> "RankingBounds", ["non-linear trans: ", (TransitionSet.to_string !nonLinearTransitions)]);
  program
    |> Program.sccs
    |> List.of_enum 
    |> fold_until (fun updated_appr scc -> improve_scc ~mrf:mrf ~cfr:cfr scc measure program (MaybeChanged.unpack updated_appr)) 
                  (fun updated_appr -> MaybeChanged.has_changed updated_appr) 
                  (MaybeChanged.return appr)