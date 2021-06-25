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


let entry_transitions_from_pre_map pre_trans_map tset =
  let all_possible_entry_trans =
    TransitionSet.enum tset
    |> Enum.fold (fun tset t -> TransitionSet.union tset @@ TransitionTable.find pre_trans_map t) TransitionSet.empty
  in
  TransitionSet.diff all_possible_entry_trans (TransitionSet.of_enum @@ TransitionSet.enum tset)


(* Computes new bounds*)
let compute_bound_mprf (appr: Approximation.t) (pre_trans_map: TransitionSet.t TransitionTable.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> entry_transitions_from_pre_map pre_trans_map
   |> TransitionSet.enum
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
                                   "non_increasing", Util.enum_to_string Transition.to_id_string (TransitionSet.enum (MultiphaseRankingFunction.non_increasing rank));
                                   "rank", MultiphaseRankingFunction.only_rank_to_string rank;])
                    ~result:Bound.to_string (fun () -> bound)


let add_bound = function
  | `Time -> Approximation.add_timebound
  | `Cost -> Approximation.add_costbound

let get_bound = function
  | `Time -> Approximation.timebound
  | `Cost -> Approximation.costbound

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
    TransitionSet.filter (fun (l,t,l') ->
      Printf.printf "one_succesor t %s  outgoing_trans %s\n" (Transition.to_id_string (l,t,l'))
        (TransitionSet.to_id_string @@ TransitionSet.of_list @@ Program.outgoing_transitions logger program [(l,t,l')]);
      List.length (Program.outgoing_transitions logger program [(l,t,l')]) == 1
    ) scc

let rec knowledge_propagation (scc: TransitionSet.t) measure program pre_trans_map appr =
  let execute () =
    scc
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum ((
      fun appr transition ->
        let new_bound =
          TransitionTable.find pre_trans_map transition
          |> TransitionSet.enum
          |> Enum.map (Approximation.timebound appr)
          |> Bound.sum
        in
        let original_bound = get_bound measure appr transition in
        if Bound.compare_asy original_bound new_bound = 1 then
          add_bound measure new_bound transition appr
          |> MaybeChanged.changed
        else
           MaybeChanged.same appr
      )) appr
    |> MaybeChanged.if_changed (knowledge_propagation scc measure program pre_trans_map)
    |> MaybeChanged.unpack
  in
  (Logger.with_log logger Logger.INFO
        (fun () -> "knowledge prop. ", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
         execute)

let improve_timebound_computation ?(inv=false) ?(fast=false) (scc: TransitionSet.t) measure program pre_trans_map max_depth appr =
  let get_unbounded_vars transition =
    Program.input_vars program
    |> VarSet.filter (Bound.is_infinity % Approximation.sizebound appr transition)
  in
  let is_time_bounded = Bound.is_finite % Approximation.timebound appr in
  let unbounded_transitions =
    scc
    |> tap (fun scc -> (Logger.log logger Logger.INFO (fun () -> "improve_timebound", ["scc", TransitionSet.to_string scc])))
    |> TransitionSet.filter (not % bounded measure appr)
  in
  let rankfunc_computation =
    if fast then
      fun depth -> MultiphaseRankingFunction.find_scc_fast ~inv:inv measure program scc depth
    else
      fun depth -> MultiphaseRankingFunction.find_scc ~inv:inv measure program pre_trans_map
        is_time_bounded get_unbounded_vars unbounded_transitions scc depth
  in
  (* Compute ranking functions up to the minimum depth such that at least one ranking functino is found
   * or the depth is max_depth *)
  (* Note that enums are lazy *)
  let rankfuncs =
    Enum.seq 1 ((+) 1) ((>) (max_depth + 1))
    |> Enum.map rankfunc_computation
    |> Enum.peek % Enum.filter (not % Enum.is_empty)
    |? Enum.empty ()
  in
  let unranked_and_unbounded =
    TransitionSet.diff unbounded_transitions
      (TransitionSet.of_enum @@ Enum.map MultiphaseRankingFunction.decreasing @@ Enum.clone rankfuncs)
  in
  nonLinearTransitions :=
    TransitionSet.union
      (TransitionSet.filter (fun t -> not (IDSet.mem (Transition.id t) !already_used_cfr)) unranked_and_unbounded)
      !nonLinearTransitions;
  rankfuncs
  |> MaybeChanged.fold_enum (fun appr -> improve_with_rank_mprf measure pre_trans_map appr) appr


let improve_timebound ?(mprf_max_depth = 1) ?(inv = false) ?(fast = false) (scc: TransitionSet.t) measure program
  pre_trans_map appr =
    let execute () = improve_timebound_computation ~inv ~fast scc measure program pre_trans_map mprf_max_depth appr in
    (Logger.with_log logger Logger.INFO
          (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
           execute)

let improve_scc rvg ?(mprf_max_depth = 1) ?(inv = false) ?(fast = false) (scc: TransitionSet.t) measure program pre_trans_map appr =
  let rec step appr =
    appr
    |> knowledge_propagation scc measure program pre_trans_map
    |> SizeBounds.improve program rvg ~scc:(Option.some scc)
    |> improve_timebound ~mprf_max_depth ~inv ~fast scc measure program pre_trans_map
    |> MaybeChanged.if_changed step
    |> MaybeChanged.unpack
  in
  (* First compute initial time bounds for the SCC and then iterate by computing size and time bounds alteratingly *)
  knowledge_propagation scc measure program pre_trans_map appr
  |> MaybeChanged.unpack % improve_timebound ~mprf_max_depth ~inv ~fast scc measure program pre_trans_map
  |> step

let log_timeout () = Logger.log logger_cfr Logger.INFO (fun () -> "TIMEOUT_CFR", ["non-linear trans", (TransitionSet.to_string !nonLinearTransitions)])

let apply_cfr ?(cfr = false) (scc: TransitionSet.t) rvg measure program appr =
  if Option.is_some !backtrack_point then (
    let (_,_,org_bound,_,_) = Option.get !backtrack_point in
    let cfr_bound = Bound.sum (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
    if (Bound.compare_asy org_bound cfr_bound) < 1 then (
      Logger.log logger_cfr Logger.INFO (fun () -> "NOT_IMPROVED", ["original bound", (Bound.to_string org_bound); "cfr bound", (Bound.show_complexity (Bound.asymptotic_complexity cfr_bound))]);
      raise NOT_IMPROVED
    );
    backtrack_point := None;
  );
  if cfr && not (TransitionSet.is_empty !nonLinearTransitions)  then
      let org_bound = Bound.sum
       (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
        backtrack_point := Option.some (program,appr,org_bound,!nonLinearTransitions, rvg);
      let opt =
        let tmp = 
          CFR.set_time_current_cfr scc appr;
          CFR.number_unsolved_trans := !CFR.number_unsolved_trans - (TransitionSet.cardinal scc);
          Logger.log logger_cfr Logger.INFO (fun () -> "RankingBounds_apply_cfr", ["non-linear trans", (TransitionSet.to_string !nonLinearTransitions); "time", string_of_float !CFR.time_current_cfr]);
          Timeout.timed_run 10. ~action:(log_timeout) (fun () -> (CFR.apply_cfr (!nonLinearTransitions) (!already_used_cfr) program appr)) in
        if Option.is_some tmp then Option.get tmp else raise TIMEOUT in
      if Option.is_some opt then (
        let (program_cfr,appr_cfr,already_used_cfr_upd) = Option.get opt in
        already_used_cfr := already_used_cfr_upd;
        Logger.log logger_cfr Logger.DEBUG (fun () -> "apply_cfr", ["already_used:", (IDSet.to_string !already_used_cfr)]);
        backtrack_point := Option.some (program,appr,org_bound,!nonLinearTransitions, rvg);
        nonLinearTransitions := TransitionSet.empty;
        let rvg_cfr = RVGTypes.RVG.rvg program_cfr in
        LocalSizeBound.switch_cache();
        LocalSizeBound.enable_cfr();
        MaybeChanged.changed (program_cfr, appr_cfr, rvg_cfr))
      else
      MaybeChanged.same (program,appr,rvg)
    else
      MaybeChanged.same (program,appr,rvg)

(* https://stackoverflow.com/questions/6749956/how-to-quit-an-iteration-in-ocaml *)
(** Fold left on a list with function [f] until predicate [p] is satisfied **)
(** In our case we restart iff a scc is unrolled *)
let rec fold_until f p acc = function
    | x :: xs when p acc -> acc
    | x :: xs -> fold_until f p (f acc x) xs
    | [] -> acc

let compute_pre_transitions_for_transition program scc =
  let table = TransitionTable.create 10 in
  TransitionSet.enum scc
  |> Enum.map (fun t -> t, TransitionSet.of_enum (Program.pre program t))
  |> Enum.iter (uncurry @@ TransitionTable.add table) (* somehow find behaves strangely in combination with of_enum *)
  |> const table

let handle_exception () = 
  LocalSizeBound.reset_cfr ();
  let (program,appr,_,non_linear_transitions,rvg_org) = Option.get !backtrack_point in
  backtrack_point := None;
  nonLinearTransitions := TransitionSet.empty;
  MaybeChanged.changed (program,appr,rvg_org)

let evaluate_program rvg ?(mprf_max_depth = 1) ?(cfr = false) ?(inv = false) ?(fast = false) measure program appr =
  program
    |> Program.sccs
    |> List.of_enum
    |> fold_until (fun monad scc ->
                        if (TransitionSet.exists (fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc) then (
                          try
                            appr
                            |> tap (const @@ Logger.log logger Logger.INFO (fun () -> "continue analysis", ["scc", TransitionSet.to_id_string scc]))
                            |> SizeBounds.improve program rvg ~scc:(Option.some scc)
                            |> improve_scc rvg ~mprf_max_depth ~inv ~fast scc measure program (compute_pre_transitions_for_transition program scc)
                            |> apply_cfr ~cfr scc rvg measure program
                          with TIMEOUT | NOT_IMPROVED ->
                            handle_exception ())
                        else monad)
                  (fun monad -> MaybeChanged.has_changed monad) (MaybeChanged.same (program,appr,rvg))

let rec improve rvg ?(mprf_max_depth = 1) ?(cfr = false) ?(inv = false) ?(fast = false) ?(currently_cfr = false) measure program appr =
    let opt = 
      if not currently_cfr then evaluate_program rvg ~mprf_max_depth ~cfr ~inv ~fast measure program appr
      else 
        let tmp = Timeout.timed_run 10. ~action:(log_timeout) (fun () -> evaluate_program rvg ~mprf_max_depth ~inv ~fast  measure program appr) in 
        if Option.is_some tmp then Option.get tmp else handle_exception () in
    opt
    |> MaybeChanged.if_changed (fun (a,b,c) -> (improve c ~cfr ~mprf_max_depth ~currently_cfr:(not currently_cfr) measure a b))
    |> MaybeChanged.unpack
