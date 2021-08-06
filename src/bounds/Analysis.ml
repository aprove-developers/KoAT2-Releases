open Batteries
open BoundsInst
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas
open CFR
open RVGTypes

let logger = Logging.(get Time)

let logger_cfr = Logging.(get CFR)

(** Table: transition -> amount of times (orginal) transition was involed in CFR. *)
let already_used_cfr = ref TransitionSet.empty

type measure = [ `Cost | `Time ] [@@deriving show]
type rvg_with_sccs = RVG.t * RVG.scc list Lazy.t

exception NOT_IMPROVED

let apply get_sizebound  = Bound.substitute_f get_sizebound % Bound.of_poly


let entry_transitions program tset =
  let all_possible_entry_trans =
    TransitionSet.enum tset
    |> Enum.fold (fun tset -> TransitionSet.union tset % Program.pre_transitionset_cached program) TransitionSet.empty
  in
  TransitionSet.diff all_possible_entry_trans (TransitionSet.of_enum @@ TransitionSet.enum tset)


(* Computes new bounds*)
let compute_bound_mprf program (appr: Approximation.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> entry_transitions program
   |> TransitionSet.enum
   |> Enum.map (fun (l,t,l') ->
       let timebound = Approximation.timebound appr (l,t,l') in
       let evaluate = apply (Approximation.sizebound appr (l,t,l')) in
       let evaluated_ranking_funcs = List.map (fun r -> evaluate @@ r l') (MultiphaseRankingFunction.rank rank) in
       let rhs = if MultiphaseRankingFunction.depth rank = 1 then
          List.nth evaluated_ranking_funcs 0
       else
          Bound.( one + (of_int (MPRF_Coefficient.coefficient rank) * Bound.sum_list evaluated_ranking_funcs) ) in
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
  let bound = compute_bound_mprf program appr rank in
  let orginal_bound = get_bound measure appr (MultiphaseRankingFunction.decreasing rank) in
  if (Bound.compare_asy orginal_bound bound) = 1 then (
    MultiphaseRankingFunction.add_to_proof rank;
    rank
    |> MultiphaseRankingFunction.decreasing
    |> (fun t -> add_bound measure bound t appr)
    |> MaybeChanged.changed)
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

let rec knowledge_propagation (scc: TransitionSet.t) measure program appr =
  let execute () =
    scc
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum ((
      fun appr transition ->
        let new_bound =
          Program.pre_transitionset_cached program transition
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
    |> MaybeChanged.if_changed (knowledge_propagation scc measure program)
    |> MaybeChanged.unpack
  in
  (Logger.with_log logger Logger.INFO
        (fun () -> "knowledge prop. ", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
         execute)

let improve_timebound_computation ?(inv=false) ?(fast=false) (scc: TransitionSet.t) measure program max_depth appr =
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
  let rankfunc_computation depth =
    let compute_function =
      if fast then
        MultiphaseRankingFunction.find_scc_fast ~inv:inv measure program scc depth
      else
        MultiphaseRankingFunction.find_scc ~inv:inv measure program is_time_bounded get_unbounded_vars scc depth
    in
    TransitionSet.to_array unbounded_transitions
    |> Parmap.array_parmap compute_function
    |> Array.enum
    |> Util.cat_maybes_enum
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
  rankfuncs
  |> MaybeChanged.fold_enum (fun appr -> improve_with_rank_mprf measure program appr) appr


let improve_timebound ?(mprf_max_depth = 1) ?(inv = false) ?(fast = false) (scc: TransitionSet.t) measure program appr =
    let execute () = improve_timebound_computation ~inv ~fast scc measure program mprf_max_depth appr in
    (Logger.with_log logger Logger.INFO
          (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
           execute)

let improve_scc rvg_with_sccs ?(mprf_max_depth = 1) ?(inv = false) ?(fast = false) (scc: TransitionSet.t) measure program appr =
  let rec step appr =
    appr
    |> knowledge_propagation scc measure program
    |> SizeBounds.improve program rvg_with_sccs ~scc:(Option.some scc)
    |> improve_timebound ~mprf_max_depth ~inv ~fast scc measure program
    |> MaybeChanged.if_changed step
    |> MaybeChanged.unpack
  in
  (* First compute initial time bounds for the SCC and then iterate by computing size and time bounds alteratingly *)
  knowledge_propagation scc measure program appr
  |> MaybeChanged.unpack % improve_timebound ~mprf_max_depth ~inv ~fast scc measure program
  |> step

let apply_cfr (scc: TransitionSet.t) rvg_with_sccs time non_linear_transitions ?(mprf_max_depth = 1) ?(inv = false) ?(fast = false) measure program appr =
  if not (TransitionSet.is_empty non_linear_transitions)  then
      let org_bound = Bound.sum (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
      let opt =
          Logger.log logger_cfr Logger.INFO
            (fun () -> "Analysis_apply_cfr", [ "non-linear trans", TransitionSet.to_string non_linear_transitions
                                                  ; "time", string_of_float time]);
          CFR.apply_cfr non_linear_transitions !already_used_cfr program appr in
      if Option.is_some opt then (
        let (program_cfr, appr_cfr, already_used_cfr_upd) = Option.get opt in
        already_used_cfr := already_used_cfr_upd;
        Logger.log logger_cfr Logger.DEBUG (fun () -> "apply_cfr", ["already_used:", (TransitionSet.to_string !already_used_cfr)]);
        Program.reset_pre_cache ();
        let rvg_with_sccs_cfr = RVGTypes.RVG.rvg_with_sccs program_cfr in
        LocalSizeBound.switch_cache();
        LocalSizeBound.enable_cfr();
        (* The new sccs which do not occur in the original program. *)
        let cfr_sccs = program_cfr
          |> Program.sccs
          |> List.of_enum
          |> List.filter (fun cfr_scc -> not (Enum.exists (fun scc_ -> TransitionSet.equal cfr_scc scc_) (Program.sccs program))) in
        let updated_appr_cfr =
          cfr_sccs
          |> List.fold_left (fun appr scc ->
                    if TransitionSet.exists (fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc then
                        appr
                        |> tap (const @@ Logger.log logger Logger.INFO (fun () -> "cfr analysis", ["scc", TransitionSet.to_id_string scc]))
                        |> SizeBounds.improve program_cfr rvg_with_sccs_cfr ~scc:(Option.some scc)
                        |> improve_scc rvg_with_sccs_cfr ~mprf_max_depth ~inv ~fast scc measure program_cfr
                    else appr)
              appr_cfr in
        let cfr_bound = Bound.sum (Enum.map
                                  (fun scc -> Bound.sum (Enum.map (fun t -> Approximation.timebound updated_appr_cfr t) (TransitionSet.enum scc)))
                                  (List.enum cfr_sccs))  in
        if (Bound.compare_asy org_bound cfr_bound) < 1 then (
          LocalSizeBound.reset_cfr();
          Program.reset_pre_cache ();
          Logger.log logger_cfr Logger.INFO (fun () -> "NOT_IMPROVED",
          ["original bound", (Bound.to_string org_bound); "cfr bound", (Bound.show_complexity (Bound.asymptotic_complexity cfr_bound))]);
          (program, appr, rvg_with_sccs)
        )
        else (
          LocalSizeBound.switch_cache();
          (program_cfr, updated_appr_cfr, rvg_with_sccs_cfr)))
      else
      (program, appr, rvg_with_sccs)
  else
    (program, appr, rvg_with_sccs)

let handle_timeout_cfr non_linear_transitions =
  LocalSizeBound.reset_cfr ();
  Program.reset_pre_cache ();
  Logger.log logger_cfr Logger.INFO (fun () -> "TIMEOUT_CFR", ["scc", (TransitionSet.to_string non_linear_transitions)])



let improve rvg ?(mprf_max_depth = 1) ?(cfr = false) ?(inv = false) ?(fast = false) measure program appr =
  program
    |> Program.sccs
    |> List.of_enum
    |> List.fold_left (fun (program, appr, rvg) scc ->
                        if TransitionSet.exists (fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc then
                            appr
                            |> tap (const @@ Logger.log logger Logger.INFO (fun () -> "continue analysis", ["scc", TransitionSet.to_id_string scc]))
                            |> SizeBounds.improve program rvg ~scc:(Option.some scc)
                            |> improve_scc rvg ~mprf_max_depth ~inv ~fast scc measure program
                            (* Apply CFR if requested; timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
                            |> fun appr ->
                                let non_linear_transitions =
                                  TransitionSet.filter (not % Bound.is_linear % Approximation.timebound appr) scc
                                  |> flip TransitionSet.diff !already_used_cfr
                                in
                              if cfr && !CFR.time_cfr > 0. && not (TransitionSet.is_empty non_linear_transitions) then (
                                let time = CFR.compute_timeout_time program appr scc in
                                let opt = Timeout.timed_run time ~action:(fun () -> handle_timeout_cfr scc)
                                (fun () -> apply_cfr scc rvg time non_linear_transitions ~mprf_max_depth ~inv ~fast measure program appr) in
                                if Option.is_some opt then
                                  let res, time_used = Option.get opt in
                                  CFR.time_cfr := !CFR.time_cfr -. time_used;
                                  res
                                else (program, appr, rvg))
                              else (program, appr, rvg)
                        else (program, appr, rvg))
                  (program, appr, rvg)
    |> Tuple3.get12
