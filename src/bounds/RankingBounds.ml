open Batteries
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas
open CFR

let logger = Logging.(get Time)

let logger_cfr = Logging.(get CFR)

let max_delta = 60

let backtrack_point = ref None

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
    if not (Bound.is_linear bound) && CFR.getLevel (MultiphaseRankingFunction.decreasing rank) < 1 then
      CFR.nonLinearTransitions := TransitionSet.add (MultiphaseRankingFunction.decreasing rank) !CFR.nonLinearTransitions
    else
      CFR.nonLinearTransitions := TransitionSet.remove (MultiphaseRankingFunction.decreasing rank) !CFR.nonLinearTransitions; 
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
            (* Printf.printf "tran: %s rhs: %s timebound %s \n" (Transition.to_id_string (RankingFunction.decreasing rank)) (Bound.to_string rhs) (Bound.to_string timebound); *)
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
    if not (Bound.is_linear bound) && CFR.getLevel (RankingFunction.decreasing rank) < 1 then 
      CFR.nonLinearTransitions := TransitionSet.add (RankingFunction.decreasing rank) !CFR.nonLinearTransitions
    else
      CFR.nonLinearTransitions := TransitionSet.remove (RankingFunction.decreasing rank) !CFR.nonLinearTransitions; 
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

(** We try to improve a single scc until we reach a fixed point. *)
let rec improve_scc ?(mrf = false) (scc: TransitionSet.t)  measure program appr =
  let execute () =
    scc
    |> TransitionSet.filter (fun t -> not (bounded measure appr t))
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum (
      (fun appr transition ->
          if mrf then
            MultiphaseRankingFunction.find measure (Option.is_some !backtrack_point) program transition
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mrf measure program appr rank) appr
          else RankingFunction.find measure (Option.is_some !backtrack_point) program transition
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank) appr)
      ) appr in 
      (Logger.with_log logger Logger.INFO
            (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
             execute)
      |> MaybeChanged.if_changed ((improve_scc ~mrf:mrf scc measure program) % (SizeBounds.improve program))
      |> MaybeChanged.unpack

let apply_cfr ?(cfr = false) ?(mrf = false)  (scc: TransitionSet.t)  measure program appr =
  if cfr && not (TransitionSet.is_empty !CFR.nonLinearTransitions) then
      let program_cfr = CFR.apply_cfr program in
      backtrack_point := Option.some (program,appr);
      if mrf then 
        MultiphaseRankingFunction.reset()
      else
        RankingFunction.reset(); 
      LocalSizeBound.reset ();  
      let appr_cfr =
      program_cfr
      |> Approximation.create
      |> TrivialTimeBounds.compute program_cfr 
      |> SizeBounds.improve program_cfr in
      MaybeChanged.changed (program_cfr,appr_cfr)
    else 
      MaybeChanged.same (program,appr)

(* https://stackoverflow.com/questions/6749956/how-to-quit-an-iteration-in-ocaml *)
(** Fold left on a list with function [f] until predicate [p] is satisfied **)
(** In our case we restart iff a scc is unrolled and start again *)
let rec fold_until f p acc = function
    | x :: xs when p acc -> acc
    | x :: xs -> fold_until f p (f acc x) xs
    | [] -> acc

let rec improve ?(mrf = false) ?(cfr = false) measure program appr =
Logger.log logger_cfr Logger.INFO (fun () -> "RankingBounds", ["non-linear trans: ", (TransitionSet.to_string !nonLinearTransitions)]);
  program
    |> Program.sccs
    |> List.of_enum
    |> fold_until (fun monad scc -> 
                        try 
                          appr
                          |> SizeBounds.improve program
                          |> improve_scc ~mrf:mrf scc measure program
                          |> apply_cfr ~cfr:cfr ~mrf:mrf scc measure program
                        with TIMEOUT ->
                          if mrf then 
                            MultiphaseRankingFunction.reset()
                          else
                            RankingFunction.reset (); 
                          LocalSizeBound.reset ();  
                          let (program,appr) = Option.get !backtrack_point in
                          backtrack_point := None;
                          MaybeChanged.changed (program,appr)) 
                  (fun monad -> MaybeChanged.has_changed monad) (MaybeChanged.same (program,appr))
    |> MaybeChanged.if_changed (fun (a,b) -> (improve ~cfr:cfr ~mrf:mrf measure a b))
    |> MaybeChanged.unpack