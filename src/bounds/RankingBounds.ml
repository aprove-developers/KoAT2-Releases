open Batteries
open BoundsInst
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas
open CFR

let logger = Logging.(get Time)

let logger_cfr = Logging.(get CFR)

let backtrack_point = ref None

(* Collect all non-linear bounds *)
let nonLinearTransitions = ref TransitionSet.empty

(** Table: transition -> amount of times (orginal) transition was involed in CFR. *)
let already_used_cfr = ref IDSet.empty

type measure = [ `Cost | `Time ] [@@deriving show, eq]

exception NOT_IMPROVED

let apply (get_sizebound: Transition.t -> Var.t -> Bound.t) (rank: Polynomial.t) (transition: Transition.t): Bound.t =
  rank
  |> Bound.of_poly
  |> Bound.substitute_f (get_sizebound transition)


(* method transforms polynome to parapolynom*)
let as_parapoly label var =
    match TransitionLabel.update label var with
    (** Correct? In the nondeterministic case we just make it deterministic? *)
    | None -> ParameterPolynomial.of_var var
    | Some p -> ParameterPolynomial.of_polynomial p

(* Computes new bounds*)
let compute_bound_mprf (appr: Approximation.t) (program: Program.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> Program.entry_transitions logger program
   |> List.enum
   |> Enum.map (fun (l,t,l') ->
       let timebound = Approximation.timebound appr (l,t,l') in
       let evaluate = (fun rank -> (apply (Approximation.sizebound appr) rank) (l,t,l')) in
       let evaluated_rankingFunctions = (List.init (MultiphaseRankingFunction.depth rank) (fun i -> (evaluate ((List.nth (MultiphaseRankingFunction.rank rank) i) l')))) in
       let rhs = if MultiphaseRankingFunction.depth rank = 1 then
          List.nth evaluated_rankingFunctions 0
       else
          Bound. (add one (mul (of_int (MPRF_Coefficient.coefficient rank))  (MPRF_Coefficient.sumBound_of_list evaluated_rankingFunctions))) in
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
    if not (Bound.is_linear bound) && not (IDSet.mem (Transition.id (MultiphaseRankingFunction.decreasing rank)) !already_used_cfr) then
      nonLinearTransitions := TransitionSet.add (MultiphaseRankingFunction.decreasing rank) !nonLinearTransitions
    else
      nonLinearTransitions := TransitionSet.remove (MultiphaseRankingFunction.decreasing rank) !nonLinearTransitions;
    Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (MultiphaseRankingFunction.decreasing rank);
                                   "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (MultiphaseRankingFunction.non_increasing rank));
                                   "rank", MultiphaseRankingFunction.only_rank_to_string rank;])
                    ~result:Bound.to_string (fun () -> bound)

 let compute_bound (appr: Approximation.t) (program: Program.t) (rank: RankingFunction.t): Bound.t =
   let execute () =
     rank
     |> RankingFunction.non_increasing
     |> Program.entry_transitions logger program
     |> List.enum
     |> Enum.map (fun (l,t,l') ->
            let timebound = Approximation.timebound appr (l,t,l') in
            let rhs = (apply (Approximation.sizebound appr) (RankingFunction.rank rank l') (l,t,l')) in
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
    if not (Bound.is_linear bound) && not (IDSet.mem (Transition.id (RankingFunction.decreasing rank)) !already_used_cfr)  then
      nonLinearTransitions := TransitionSet.add (RankingFunction.decreasing rank) !nonLinearTransitions
    else
      nonLinearTransitions := TransitionSet.remove (RankingFunction.decreasing rank) !nonLinearTransitions;
   Logger.with_log logger Logger.DEBUG
        (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (RankingFunction.decreasing rank);
                                     "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (RankingFunction.non_increasing rank));
                                     "rank", RankingFunction.only_rank_to_string rank])
                      ~result:Bound.to_string (fun () -> bound)

let add_bound = function
  | `Time -> Approximation.add_timebound
  | `Cost -> Approximation.add_costbound

let get_bound = function
  | `Time -> Approximation.timebound
  | `Cost -> Approximation.costbound

let improve_with_rank  measure program appr rank =
  let bound = compute_bound appr program rank in
  let orginal_bound = get_bound measure appr (RankingFunction.decreasing rank) in
  if (Bound.compare_asy orginal_bound bound) = 1 then
    rank
    |> RankingFunction.decreasing
    |> (fun t -> add_bound measure bound t appr)
    |> MaybeChanged.changed
  else
    MaybeChanged.same appr

let improve_with_rank_mprf measure program appr rank =
  let bound = compute_bound_mprf appr program rank in
  let orginal_bound = get_bound measure appr (MultiphaseRankingFunction.decreasing rank) in
  if (Bound.compare_asy orginal_bound bound) = 1 then
    rank
    |> MultiphaseRankingFunction.decreasing
    |> (fun t -> add_bound measure bound t appr)
    |> MaybeChanged.changed
  else
    MaybeChanged.same appr

(** Checks if a transition is bounded *)
let bounded measure appr transition =
  match measure with
  | `Time -> Approximation.is_time_bounded appr transition
  | `Cost -> if Polynomial.is_const (Transition.cost transition) then
        true (* We can not compute a better bound in this case, so we consider this transition as bounded *)
      else
        false

let one_successor (program: Program.t) (scc: TransitionSet.t) =
    TransitionSet.filter (fun (l,g,l') -> (not (Location.equal l l'))
                                       && (List.length (Program.outgoing_transitions logger program [(l,g,l')])) == 1) scc

let knowledge_propagation (scc: TransitionSet.t) measure program appr =
  let execute () =
  scc
  |> one_successor program
  |> TransitionSet.enum
  |> MaybeChanged.fold_enum ((
    fun appr transition ->
      let new_bound =
      [transition]
      |> Program.entry_transitions logger program
      |> List.enum
      |> Enum.map (fun (l,t,l') ->
        Approximation.timebound appr (l,t,l'))
      |> Bound.sum in
      let orginal_bound = get_bound measure appr transition in
      if (Bound.compare_asy orginal_bound new_bound) = 1 then
        add_bound measure new_bound transition appr
        |> MaybeChanged.changed
      else
         MaybeChanged.same appr
      )) appr in
      (Logger.with_log logger Logger.INFO
            (fun () -> "knowledge prop. ", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
             execute)

(** We try to improve a single scc until we reach a fixed point. *)
let rec improve_timebound_rec cache_rf cache_mprf ?(mprf = false) ?(inv = false) ?(fast = false) (scc: TransitionSet.t)  measure program appr =
  let execute () =
    scc
    |> tap (fun scc -> (Logger.with_log logger_cfr Logger.INFO
            (fun () -> "scc", ["scc", TransitionSet.to_string scc])
             (fun _ -> ())))
    |> TransitionSet.filter (fun t -> not (bounded measure appr t))
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum (
      (fun appr transition ->
          if mprf then
            (if fast then
              MultiphaseRankingFunction.find_scc_fast ~inv:inv cache_mprf measure (Option.is_some !backtrack_point) program transition scc
            else
              MultiphaseRankingFunction.find_scc ~inv:inv cache_mprf measure (Option.is_some !backtrack_point) program transition scc)
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mprf measure program appr rank) appr
          else
            (if fast then
              RankingFunction.find_scc_fast cache_rf ~inv:inv measure (Option.is_some !backtrack_point) program transition scc
            else
              RankingFunction.find_scc cache_rf ~inv:inv measure (Option.is_some !backtrack_point) program transition scc)
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank) appr)
      ) appr in
      (Logger.with_log logger Logger.INFO
            (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
             execute)
      |> MaybeChanged.if_changed (improve_timebound_rec cache_rf cache_mprf ~mprf:mprf ~inv:inv ~fast:fast scc measure program)
      |> MaybeChanged.unpack

  let improve_timebound cache_rf cache_mprf ?(mprf = false) ?(inv = false) ?(fast = false) (scc: TransitionSet.t)  measure program appr =
  let execute () =
    scc
    |> tap (fun scc -> (Logger.with_log logger_cfr Logger.INFO
            (fun () -> "improve_timebound", ["scc", TransitionSet.to_string scc])
             (fun _ -> ())))
    |> TransitionSet.filter (fun t -> not (bounded measure appr t))
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum (
      (fun appr transition ->
          if mprf then
            (if fast then
              MultiphaseRankingFunction.find_scc_fast ~inv:inv cache_mprf measure (Option.is_some !backtrack_point) program transition scc
            else
              MultiphaseRankingFunction.find_scc ~inv:inv cache_mprf measure (Option.is_some !backtrack_point) program transition scc)
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mprf measure program appr rank) appr
          else
            (if fast then
              RankingFunction.find_scc_fast cache_rf ~inv:inv measure (Option.is_some !backtrack_point) program transition scc
            else
              RankingFunction.find_scc cache_rf ~inv:inv measure (Option.is_some !backtrack_point) program transition scc)
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank) appr)
      ) appr in
      (Logger.with_log logger Logger.INFO
            (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
             execute)

  let rec improve_scc rvg cache_rf cache_mprf ?(mprf = false) ?(inv = false) ?(fast = false) (scc: TransitionSet.t)  measure program appr =
    appr
    |> improve_timebound_rec cache_rf cache_mprf ~mprf:mprf ~inv:inv ~fast:fast scc measure program
    |> SizeBounds.improve program rvg ~scc:(Option.some scc) (Option.is_some !backtrack_point)
    |> improve_timebound cache_rf cache_mprf ~mprf:mprf ~inv:inv ~fast:fast scc measure program
    |> MaybeChanged.if_changed (improve_scc rvg cache_rf cache_mprf ~mprf:mprf ~inv:inv ~fast:fast scc measure program)
    |> MaybeChanged.unpack
    |> knowledge_propagation scc measure program
    |> MaybeChanged.unpack


let apply_cfr ?(cfr = false) ?(mprf = false) (scc: TransitionSet.t) measure program appr =
  if Option.is_some !backtrack_point then (
    let (_,_,org_bound,_) = Option.get !backtrack_point in
    let cfr_bound = Bound.sum (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
    if (Bound.compare_asy org_bound cfr_bound) < 1 then (
      Logger.log logger_cfr Logger.INFO (fun () -> "NOT_IMPROVED", ["orginial bound", (Bound.to_string org_bound); "cfr bound", (Bound.show_complexity (Bound.asymptotic_complexity cfr_bound))]);
      raise NOT_IMPROVED
    );
    backtrack_point := None;
  );
  if cfr && not (TransitionSet.is_empty !nonLinearTransitions)  then
      let org_bound = Bound.sum
       (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
        backtrack_point := Option.some (program,appr,org_bound,!nonLinearTransitions);
      let opt =
        CFR.set_time_current_cfr scc appr;
        CFR.number_unsolved_trans := !CFR.number_unsolved_trans - (TransitionSet.cardinal scc);
        Logger.log logger_cfr Logger.INFO (fun () -> "RankingBounds_apply_cfr", ["non-linear trans", (TransitionSet.to_string !nonLinearTransitions); "time", string_of_float !CFR.time_current_cfr]);
        CFR.apply_cfr (!nonLinearTransitions) (!already_used_cfr) program appr in
      if Option.is_some opt then (
        let (program_cfr,appr_cfr,already_used_cfr_upd) = Option.get opt in
        already_used_cfr := already_used_cfr_upd;
        backtrack_point := Option.some (program,appr,org_bound,!nonLinearTransitions);
        LocalSizeBound.switch_cache();
        LocalSizeBound.enable_cfr();
        MaybeChanged.changed (program_cfr, appr_cfr))
      else
      MaybeChanged.same (program,appr)
    else
      MaybeChanged.same (program,appr)

(* https://stackoverflow.com/questions/6749956/how-to-quit-an-iteration-in-ocaml *)
(** Fold left on a list with function [f] until predicate [p] is satisfied **)
(** In our case we restart iff a scc is unrolled and start again *)
let rec fold_until f p acc = function
    | x :: xs when p acc -> acc
    | x :: xs -> fold_until f p (f acc x) xs
    | [] -> acc

let rec improve cache_rf rvg cache_mprf ?(mprf = false) ?(cfr = false) ?(inv = false) ?(fast = false) measure program appr =
  program
    |> Program.sccs
    |> List.of_enum
    |> fold_until (fun monad scc ->
                        if (TransitionSet.exists (fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc) then (
                          if mprf then
                            MultiphaseRankingFunction.reset cache_mprf
                          else
                            RankingFunction.reset cache_rf;
                          try
                            appr
                            |> SizeBounds.improve program rvg ~scc:(Option.some scc) (Option.is_some !backtrack_point)
                            |> improve_scc rvg cache_rf cache_mprf ~mprf:mprf ~inv:inv ~fast:fast scc measure program
                            |> apply_cfr ~cfr:cfr ~mprf:mprf scc measure program
                          with TIMEOUT | NOT_IMPROVED ->
                            LocalSizeBound.reset_cfr ();
                            let (program,appr,_,non_linear_transitions) = Option.get !backtrack_point in
                            backtrack_point := None;
                            nonLinearTransitions := non_linear_transitions;
                            MaybeChanged.changed (program,appr))
                        else monad)
                  (fun monad -> MaybeChanged.has_changed monad) (MaybeChanged.same (program,appr))
    |> MaybeChanged.if_changed (fun (a,b) -> (improve cache_rf rvg cache_mprf ~cfr:cfr ~mprf:mprf measure a b))
    |> MaybeChanged.unpack
