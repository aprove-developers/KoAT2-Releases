open Atoms
open Batteries
open BoundsInst
open Constraints
open Formulas
open PartialEvaluation
open Polynomials
open ProgramModules
open RVGTypes

(* The types below are used to restrict certain analyses methods to certain underlying types *)
(* TODO simpliy somehow? *)
type ('prog,'tset,'rvg,'rvg_scc,'twn,'appr) cfr_configuration =
  | NoCFR: ('a,'b,'c,'d,'e,'f) cfr_configuration
  | PerformCFR: [ `Chaining | `PartialEvaluation ] list
              -> ( Program.t
                , TransitionSet.t
                , RVGTypes.MakeRVG(ProgramModules).t, RVGTypes.MakeRVG(ProgramModules).scc
                , Loop.Make(ProgramModules).t
                , Approximation.MakeForClassicalAnalysis(ProgramModules).t) cfr_configuration

type ('prog, 'tset, 'appr) closed_form_size_bounds =
  | NoClosedFormSizeBounds: ('prog,'trans_set,'appr) closed_form_size_bounds
  | ComputeClosedFormSizeBounds: (Program.t, TransitionSet.t,Approximation.MakeForClassicalAnalysis(ProgramModules).t) closed_form_size_bounds

type ('trans,'prog,'tset,'rvg,'rvg_scc,'twn,'appr) analysis_configuration =
  { run_mprf_depth: int option
  ; twn_configuration: TWN.configuration option
  ; cfr_configuration: ('prog,'tset,'rvg,'rvg_scc,'twn,'appr) cfr_configuration
  ; closed_form_size_bounds: ('prog, 'tset,'appr) closed_form_size_bounds
  }

type classical_program_conf_type = ( Transition.t
                                   , Program.t
                                   , TransitionSet.t
                                   , RVGTypes.MakeRVG(ProgramModules).t
                                   , RVGTypes.MakeRVG(ProgramModules).scc
                                   , Loop.Make(ProgramModules).t
                                   , Approximation.MakeForClassicalAnalysis(ProgramModules).t ) analysis_configuration


type measure = [ `Cost | `Time ] [@@deriving show]

let logger = Logging.(get Time)

let logger_cfr = Logging.(get CFR)

let termination = ref false
let termination_only t = termination := t
let relax_loops = ref false
let only_relax_loops t = relax_loops := t

let default_configuration: ('a,'b,'c,'d,'e,'f,'g) analysis_configuration =
  { run_mprf_depth = Some 1
  ; twn_configuration = None
  ; cfr_configuration = NoCFR
  ; closed_form_size_bounds = NoClosedFormSizeBounds }


module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Approximation_ = Approximation
  module Approximation = Approximation.MakeForClassicalAnalysis(PM)
  module CostBounds = CostBounds.Make(PM)
  module LSB = LocalSizeBound.Make(PM.TransitionLabel)(PM.Transition)(PM.Program)
  module LSB_Table = Hashtbl.Make(PM.RV.RVTuple_)
  module MultiphaseRankingFunction = MultiphaseRankingFunction.Make(PM)
  module RVG = RVGTypes.MakeRVG(PM)
  module SizeBounds = SizeBounds.Make(PM)
  module SizeBounds_ = SizeBounds
  module TWN = TWN.Make(PM)

  type conf_type =
    (Transition.t,Program.t,TransitionSet.t,RVG.t,RVG.scc,Loop.Make(PM).t, Approximation.t) analysis_configuration
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
       let depth = MultiphaseRankingFunction.depth rank in
       let rhs =
         if depth = 1 then
           List.nth evaluated_ranking_funcs 0
         else
           Bound.(one + (of_int (MPRF_Coefficient.coefficient depth) * Bound.sum_list evaluated_ranking_funcs))
       in
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

let bounded_mprf program (appr: Approximation.t) (rank: MultiphaseRankingFunction.t): bool =
  let execute () =
    rank
    |> MultiphaseRankingFunction.non_increasing
    |> entry_transitions program
    |> TransitionSet.enum
    |> Enum.for_all (fun (l,t,l') ->
      let timebound = Approximation.timebound appr (l,t,l') in
      let evaluated_ranking_funcs = List.map (fun r -> Bound.of_poly @@ r l') (MultiphaseRankingFunction.rank rank) in
      let depth = MultiphaseRankingFunction.depth rank in
      if depth = 1 then
        Bound.is_finite timebound || Bound.equal Bound.zero  @@ List.first evaluated_ranking_funcs
      else
        Bound.is_finite timebound && List.for_all Bound.is_finite evaluated_ranking_funcs)
  in
      let terminates = execute () in
      Logger.with_log logger Logger.DEBUG
        (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (MultiphaseRankingFunction.decreasing rank);
                                    "non_increasing", Util.enum_to_string Transition.to_id_string (TransitionSet.enum (MultiphaseRankingFunction.non_increasing rank));
                                    "rank", MultiphaseRankingFunction.only_rank_to_string rank;])
                     ~result:(fun b -> if b then "yes" else "maybe") 
                     (fun () -> terminates) 
                                      
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
    MultiphaseRankingFunction.add_to_proof rank (Some bound) program;
    rank
    |> MultiphaseRankingFunction.decreasing
    |> (fun t -> add_bound measure bound t appr)
    |> MaybeChanged.changed)
  else
    MaybeChanged.same appr

let improve_with_twn program scc transformation_type appr =
  let compute appr_ t =
   let bound = TWN.time_bound ~relax_loops:!relax_loops transformation_type t scc program appr_ in
   let orginal_bound = get_bound `Time appr_ t in
    if (Bound.compare_asy orginal_bound bound) = 1 then
      MaybeChanged.changed (add_bound `Time bound t appr_)
    else
      MaybeChanged.same appr_ in
  MaybeChanged.fold compute appr (TransitionSet.to_list (TransitionSet.filter (Bound.is_infinity % Approximation.timebound appr) scc))

(** Checks if a transition is bounded *)
let bounded measure appr transition =
  match measure with
  | `Time -> Approximation.is_time_bounded appr transition
  | `Cost -> Polynomial.is_const (Transition.cost transition) (* We can not compute a better bound in this case, so we consider this transition as bounded *)

let improve_termination_twn program scc transformation_type appr =
  let compute appr_ t =
    let terminates = TWN.terminates ~relax_loops:!relax_loops transformation_type t scc program appr_ in
    let orginal_terminates = bounded `Time appr_ t in
    if not orginal_terminates && terminates then
      MaybeChanged.changed (add_bound `Time Bound.one t appr_)
    else
      MaybeChanged.same appr_ in
  MaybeChanged.fold compute appr (TransitionSet.to_list (TransitionSet.filter (Bound.is_infinity % Approximation.timebound appr) scc))
  

let improve_termination_rank_mprf measure program appr rank =
  let terminates = bounded_mprf program appr rank in
  let orginal_terminates = bounded measure appr (MultiphaseRankingFunction.decreasing rank) in
  if not orginal_terminates && terminates then (
    let dummy_bound = if terminates then Bound.one else Bound.infinity in
    MultiphaseRankingFunction.add_to_proof rank None program;
    rank
    |> MultiphaseRankingFunction.decreasing
    |> (fun t -> add_bound measure dummy_bound t appr)
    |> MaybeChanged.changed)
  else
    MaybeChanged.same appr
  

let rec knowledge_propagation (scc: TransitionSet.t) program appr =
  let execute () =
    scc
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum (
      fun appr transition ->
        let new_bound =
          Program.pre_transitionset_cached program transition
          |> TransitionSet.enum
          |> Enum.map (Approximation.timebound appr)
          |> Bound.sum
        in
        let original_bound = get_bound `Time appr transition in
        if Bound.compare_asy original_bound new_bound = 1 then (
          ProofOutput.add_str_paragraph_to_proof (fun () ->
            "knowledge_propagation leads to new time bound "^Bound.to_string ~pretty:true new_bound^" for transition "^Transition.to_string_pretty transition
          );
          add_bound `Time new_bound transition appr
          |> MaybeChanged.changed)
        else
           MaybeChanged.same appr
      ) appr
    |> MaybeChanged.if_changed (knowledge_propagation scc program)
    |> MaybeChanged.unpack
  in
  (Logger.with_log logger Logger.INFO
        (fun () -> "knowledge prop. ", ["scc", TransitionSet.to_string scc])
         execute)

let rec knowledge_propagation_size (scc: TransitionSet.t) program appr =
  let execute () =
    scc
    |> TransitionSet.to_list
    |> List.cartesian_product (VarSet.to_list @@ Program.input_vars program)
    |> List.filter (fun (var,transition) -> Bound.is_infinity @@ Approximation.sizebound appr transition var)
    |> List.enum
    |> MaybeChanged.fold_enum (
      fun appr (var,(l,t,l')) ->
        let new_bound =
          Program.pre_transitionset_cached program (l,t,l')
          |> TransitionSet.enum
          |> Enum.map (flip (Approximation.sizebound appr) var)
          |> Bound.sum
          |> Bound.substitute_f (fun var -> Bound.of_poly @@ (TransitionLabel.update t var |? Polynomial.of_var var))
        in
        let original_bound = Approximation.sizebound appr (l,t,l') var in
        if Bound.compare_asy original_bound new_bound = 1 then (
          ProofOutput.add_str_paragraph_to_proof (fun () ->
            "knowledge_propagation leads to new size bound "^Bound.to_string ~pretty:true new_bound^" for var" ^ Var.to_string var ^ " and transition " ^ Transition.to_string_pretty (l,t,l')
          );
          Approximation.add_sizebound new_bound (l,t,l') var appr
          |> MaybeChanged.changed)
        else
           MaybeChanged.same appr
      ) appr
    |> MaybeChanged.if_changed (knowledge_propagation_size scc program)
    |> MaybeChanged.unpack
  in
  (Logger.with_log logger Logger.INFO
        (fun () -> "knowledge prop. ", ["scc", TransitionSet.to_string scc])
         execute)

let local_rank (scc: TransitionSet.t) measure program max_depth appr =
    let get_unbounded_vars transition =
      program
      |> (if !termination then Program.vars else Program.input_vars)
      |> VarSet.filter (Bound.is_infinity % Approximation.sizebound appr transition)
    in
    let is_time_bounded = Bound.is_finite % Approximation.timebound appr in
    let unbounded_transitions =
      scc
      |> tap (fun scc -> (Logger.log logger Logger.INFO (fun () -> "improve_timebound", ["scc", TransitionSet.to_string scc])))
      |> TransitionSet.filter (not % bounded measure appr)
    in
    let scc_overapprox_nonlinear = TransitionSet.map Transition.overapprox_nonlinear_updates scc in
    let rankfunc_computation depth =
      let compute_function =
        MultiphaseRankingFunction.find_scc ~termination_only:!termination measure program is_time_bounded get_unbounded_vars scc_overapprox_nonlinear depth
          % flip TransitionSet.find scc_overapprox_nonlinear
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
      |? Enum.empty ()    in
    let improvement_function = if !termination then improve_termination_rank_mprf else improve_with_rank_mprf in
    rankfuncs
    |> MaybeChanged.fold_enum (fun appr -> improvement_function measure program appr) appr


let run_local ~conf (scc: TransitionSet.t) measure program appr =
  MaybeChanged.(
    return appr
    >>= fun appr ->
      (match conf.run_mprf_depth with
        | Some max_depth -> local_rank scc measure program max_depth appr
        | None           -> MaybeChanged.return appr )
    >>= fun appr ->
      (match measure, conf.twn_configuration with
        | (`Cost,_) -> MaybeChanged.return appr
        | (`Time,None) -> MaybeChanged.return appr
        | (`Time,Some twn_conf) -> if !termination
            then improve_termination_twn program scc twn_conf appr
            else improve_with_twn program scc twn_conf appr)
  )

let improve_timebound (scc: TransitionSet.t) measure program appr =
    let execute () = run_local scc measure program appr in
    (Logger.with_log logger Logger.INFO
          (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
           execute)

let twn_size_bounds ~(conf: conf_type) (scc: TransitionSet.t) (program: Program.t) (appr: Approximation.t) =
  match conf.closed_form_size_bounds with
  | NoClosedFormSizeBounds -> appr
  | ComputeClosedFormSizeBounds ->
    TWNSizeBounds.improve program ~scc:(Option.some scc) appr
    |> SolvableSizeBounds.improve program ~scc:(Option.some scc)

(* TODO unify conf types with ~local! *)
let improve_scc ~conf rvg_with_sccs (scc: TransitionSet.t) program lsb_table appr =
  let rec step appr =
    appr
    |> knowledge_propagation scc program
    |> SizeBounds.improve program rvg_with_sccs ~scc:(Option.some scc) (LSB_Table.find lsb_table)
    |> twn_size_bounds ~conf scc program
    |> knowledge_propagation_size scc program
    |> improve_timebound ~conf scc `Time program
    |> MaybeChanged.if_changed step
    |> MaybeChanged.unpack
  in
  (* First compute initial time bounds for the SCC and then iterate by computing size and time bounds alteratingly *)
  knowledge_propagation scc program appr
  |> MaybeChanged.unpack % improve_timebound ~conf scc `Time program
  |> step

let scc_cost_bounds ~conf program scc appr =
  if TransitionSet.exists (not % Polynomial.is_const % Transition.cost) scc then
    MaybeChanged.unpack (improve_timebound ~conf scc `Cost program appr)
  else
    appr


let handle_timeout_cfr method_name non_linear_transitions =
  Program.reset_pre_cache ();
  TWN.reset_cfr();
  TWNSizeBounds.reset_cfr();
  SolvableSizeBounds.reset_cfr();
  Logger.log logger_cfr Logger.INFO (fun () -> "TIMEOUT_CFR_" ^ method_name, ["scc", (TransitionSet.to_string non_linear_transitions)])


  let all_rvs program input_vars =
    List.cartesian_product
      (TransitionSet.to_list (Program.transitions program))
      (VarSet.to_list input_vars)

  let add_missing_lsbs program lsbs =
    let input_vars = program |> if !termination then Program.vars else Program.input_vars in
    all_rvs program input_vars
    |> List.iter (fun(t,v)->
        if LSB_Table.mem lsbs (t,v) then ()
        else LSB_Table.add lsbs (t,v) (LSB.compute_bound input_vars t v)
       )

  let compute_lsbs program =
    let vars = program |> if !termination then Program.vars else Program.input_vars in
    List.enum (all_rvs program vars)
    |> Enum.map (fun(t,v) -> (t,v),LSB.compute_bound vars t v)
    |> LSB_Table.of_enum


  let handle_cfr ~(conf: conf_type) ~(preprocess: Program.t -> Program.t) (scc: TransitionSet.t)
    program rvg lsbs appr: Program.t * Approximation.t * (RVG.t * RVG.scc list lazy_t) =
    match conf.cfr_configuration with
    | NoCFR -> program,appr,rvg
    | PerformCFR cfr -> (
      let module Approximation = Approximation_ in
      let module SizeBounds = SizeBounds_ in
      let apply_cfr
          (method_name: string)
          (f_cfr: Program.t -> Program.t MaybeChanged.t)
          (f_proof: Program.t -> Bound.t -> unit)
          (rvg_with_sccs: RVGTypes.MakeRVG(ProgramModules).t * RVGTypes.MakeRVG(ProgramModules).scc list Lazy.t)
          (time: float)
          (non_linear_transitions: TransitionSet.t)
          ~(preprocess: Program.t -> Program.t)
          (program: Program.t)
          (appr: Approximation_.MakeForClassicalAnalysis(ProgramModules).t) =

        if not (TransitionSet.is_empty non_linear_transitions) then
          let org_bound = Bound.sum (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
          let mc =
            Logger.log logger_cfr Logger.INFO
              (fun () -> "Analysis_apply_" ^ method_name, [ "non-linear trans", TransitionSet.to_string non_linear_transitions; "time", string_of_float time]);
            f_cfr program in
          if MaybeChanged.has_changed mc then (
            ProofOutput.add_to_proof (fun () -> FormattedString.mk_str_header_big "Analysing control-flow refined program");
            let program_cfr = mc |> MaybeChanged.unpack |> preprocess  in
            add_missing_lsbs program_cfr lsbs;
            Logger.log logger_cfr Logger.DEBUG (fun () -> "apply_" ^ method_name, []);
            Program.reset_pre_cache ();
            TWN.reset_cfr();
            TWNSizeBounds.reset_cfr();
            SolvableSizeBounds.reset_cfr();
            let rvg_with_sccs_cfr = RVG.rvg_with_sccs (Option.map (LSB.vars % Tuple2.first) % LSB_Table.find lsbs) program_cfr in
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
                    |> tap (const @@ Logger.log logger Logger.INFO (fun () -> method_name ^ "analysis", ["scc", TransitionSet.to_id_string scc]))
                    |> SizeBounds.improve program_cfr rvg_with_sccs_cfr ~scc:(Option.some scc) (LSB_Table.find lsbs)
                    |> twn_size_bounds ~conf scc program_cfr
                    |> improve_scc ~conf rvg_with_sccs_cfr scc program_cfr lsbs
                  else appr)
                (CFR.merge_appr program program_cfr appr) in
            let cfr_bound = Bound.sum (Enum.map
                                        (fun scc -> Bound.sum (Enum.map (fun t -> Approximation.timebound updated_appr_cfr t) (TransitionSet.enum scc)))
                                        (List.enum cfr_sccs))  in
            if (Bound.compare_asy org_bound cfr_bound) < 1 then (
              ProofOutput.add_to_proof (fun () -> FormattedString.mk_str_header_big "CFR did not improve the program. Rolling back");
              Program.reset_pre_cache ();
              TWN.reset_cfr();
              TWNSizeBounds.reset_cfr();
              SolvableSizeBounds.reset_cfr();
              Logger.log logger_cfr Logger.INFO (fun () -> "NOT_IMPROVED",
                                                          ["original bound", (Bound.to_string org_bound); method_name ^ " bound", (Bound.show_complexity (Bound.asymptotic_complexity cfr_bound))]);
              (program, appr, rvg_with_sccs)
            )
            else (
              f_proof program_cfr cfr_bound;
              (program_cfr, updated_appr_cfr, rvg_with_sccs_cfr)))
          else
            (program, appr, rvg_with_sccs)
        else
          (program, appr, rvg_with_sccs)
      in
      let non_linear_transitions =
        TransitionSet.filter (not % Bound.is_linear % Approximation.timebound appr) scc
      in
      if not (TransitionSet.is_empty non_linear_transitions) && List.mem `Chaining cfr then (
        let opt = Timeout.timed_run 10. ~action:(fun () -> handle_timeout_cfr "partial_evaluation" scc)
            (fun () -> apply_cfr "chaining" (CFR.lift_to_program (Chaining.transform_graph ~scc:(Option.some scc))) (fun _ _ -> ()) rvg 10. non_linear_transitions ~preprocess program appr) in
        if Option.is_some opt then
          let res, time_used = Option.get opt in
          res
        else (program, appr, rvg))
      else (program, appr, rvg)
      |> fun (program, appr, rvg) -> (
        let non_linear_transitions =
          TransitionSet.filter (not % Bound.is_linear % Approximation.timebound appr) (TransitionSet.inter scc (Program.transitions program))
        in
        if !PartialEvaluation.time_cfr > 0. && not (TransitionSet.is_empty non_linear_transitions) && List.mem `PartialEvaluation cfr then (
          let time = PartialEvaluation.compute_timeout_time program appr scc in
          let opt = Timeout.timed_run time ~action:(fun () -> handle_timeout_cfr "partial_evaluation" scc)
              (fun () -> apply_cfr "partial_evaluation" (PartialEvaluation.apply_cfr non_linear_transitions) (PartialEvaluation.add_to_proof) rvg time non_linear_transitions ~preprocess program appr) in
          if Option.is_some opt then
            let res, time_used = Option.get opt in
            PartialEvaluation.time_cfr := !PartialEvaluation.time_cfr -. time_used;
            res
          else (program, appr, rvg))
        else (program, appr, rvg))
    )

  let improve ~conf ~preprocess program appr =
    (* let appr = List.fold (fun appr (v,t) -> Approximation.add_sizebound (Bound.of_var v) t v appr) appr (List.cartesian_product (Program.input_vars program |> VarSet.to_list) (Program.transitions program |> TransitionSet.to_list)) in *) (*TODO Remove and make unhacky (heuristic entry transition terminates instead of sizebounds)*)
    let lsbs = compute_lsbs program in
    let rvg = RVG.rvg_with_sccs (Option.map (LSB.vars % Tuple2.first)% LSB_Table.find lsbs) program in
    let program, appr =
      program
      |> Program.sccs
      |> List.of_enum
      |> List.fold_left (fun (program, appr, rvg) scc_orig ->
             let improve_scc scc =
               if TransitionSet.exists (fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc then
                 appr
                 |> tap (const @@ Logger.log logger Logger.INFO (fun () -> "continue analysis", ["scc", TransitionSet.to_id_string scc]))
                 |> SizeBounds.improve program rvg ~scc:(Option.some scc) (LSB_Table.find lsbs)
                 |> twn_size_bounds ~conf scc program
                 |> improve_scc ~conf rvg scc program lsbs
                 (* Apply CFR if requested; timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
                 |> handle_cfr ~conf ~preprocess scc program rvg lsbs
                 |> fun (program,appr,rvg) -> (program, scc_cost_bounds ~conf program scc appr,rvg)
               else (program, appr, rvg)
             in
             (* Check if SCC still exists and keep only existing transitions (Preprocessing in cfr might otherwise cut them ) *)
             match conf.cfr_configuration with
             | NoCFR        -> improve_scc scc_orig
             | PerformCFR _ ->
                let scc = TransitionSet.inter scc_orig (Program.transitions program) in
                if TransitionSet.is_empty scc then (program,appr,rvg)
                else improve_scc scc
           ) (program, appr, rvg)
      |> Tuple3.get12
    in
    let appr_with_costbounds = CostBounds.infer_from_timebounds program appr in
    program, appr_with_costbounds
end
