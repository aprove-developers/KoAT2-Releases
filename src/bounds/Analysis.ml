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
type ('prog,'trans_compare,'rvg,'rvg_scc,'twn,'appr) cfr_configuration =
  | NoCFR: ('a,'b,'c,'d,'e,'f) cfr_configuration
  | PerformCFR: [ `Chaining | `PartialEvaluation ] list
              -> ( Program.t
                , Transition.comparator_witness
                , RVGTypes.MakeRVG(ProgramModules).t, RVGTypes.MakeRVG(ProgramModules).scc
                , Loop.Make(ProgramModules).t
                , Approximation.MakeForClassicalAnalysis(ProgramModules).t) cfr_configuration

type ('trans, 'prog, 'trans_compare, 'appr) closed_form_size_bounds =
  | NoClosedFormSizeBounds: ('trans, 'prog,'trans_compare,'appr) closed_form_size_bounds
  | ComputeClosedFormSizeBounds: (Transition.t, Program.t,Transition.comparator_witness,Approximation.MakeForClassicalAnalysis(ProgramModules).t) closed_form_size_bounds

type ('trans,'prog,'trans_compare,'rvg,'rvg_scc,'twn,'appr) analysis_configuration =
  { run_mprf_depth: int option
  ; twn_configuration: TWN.configuration option
  ; cfr_configuration: ('prog,'trans_compare,'rvg,'rvg_scc,'twn,'appr) cfr_configuration
  ; closed_form_size_bounds: ('trans, 'prog, 'trans_compare,'appr) closed_form_size_bounds
  ; analysis_type: [`Termination | `Complexity]
  }

type classical_program_conf_type = ( Transition.t
                                   , Program.t
                                   , Transition.comparator_witness
                                   , RVGTypes.MakeRVG(ProgramModules).t
                                   , RVGTypes.MakeRVG(ProgramModules).scc
                                   , Loop.Make(ProgramModules).t
                                   , Approximation.MakeForClassicalAnalysis(ProgramModules).t ) analysis_configuration


type measure = [ `Cost | `Time ] [@@deriving show]

let logger = Logging.(get Time)

let logger_cfr = Logging.(get CFR)

let default_configuration: ('a,'b,'c,'d,'e,'f,'g) analysis_configuration =
  { run_mprf_depth = Some 1
  ; twn_configuration = None
  ; cfr_configuration = NoCFR
  ; closed_form_size_bounds = NoClosedFormSizeBounds
  ; analysis_type = `Complexity}


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
    (Transition.t,Program.t,Transition.comparator_witness,RVG.t,RVG.scc,Loop.Make(PM).t, Approximation.t) analysis_configuration

  let proof_program_is_standard
      ~(eq_proof_prog:(Program.t, ProgramModules.Program.t) Util.TypeEq.t): (Program.t, ProgramModules.Program.t) Util.TypeEq.t =
    eq_proof_prog

  let proof_tset_is_standard
      ~(eq_proof_trans:(Transition.t, ProgramModules.Transition.t) Util.TypeEq.t)
      ~(eq_proof_trans_comparator:(Transition.comparator_witness, ProgramModules.Transition.comparator_witness) Util.TypeEq.t):
        (TransitionSet.t, ProgramModules.TransitionSet.t) Util.TypeEq.t =
    let Refl = eq_proof_trans in let Refl = eq_proof_trans_comparator in Refl

let apply get_sizebound  = Bound.substitute_f get_sizebound % Bound.of_poly


let entry_transitions program tset =
  let all_possible_entry_trans =
    Base.Set.to_sequence tset
    |> Base.Sequence.fold ~f:(fun tset -> Base.Set.union tset % Program.pre_transitionset_cached program) ~init:TransitionSet.empty
  in
  Base.Set.diff all_possible_entry_trans tset

(* Computes new bounds*)
let compute_bound_mprf program (appr: Approximation.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> entry_transitions program
   |> Base.Set.to_sequence
   |> Base.Sequence.map ~f:(fun (l,t,l') ->
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
   |> Bound.sum_sequence
 in
    let bound = execute () in
    Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (MultiphaseRankingFunction.decreasing rank);
                                   "non_increasing", Util.sequence_to_string ~f:Transition.to_id_string (Base.Set.to_sequence (MultiphaseRankingFunction.non_increasing rank));
                                   "rank", MultiphaseRankingFunction.only_rank_to_string rank;])
                    ~result:Bound.to_string (fun () -> bound)

let bounded_mprf program (appr: Approximation.t) (rank: MultiphaseRankingFunction.t): bool =
  let execute () =
    rank
    |> MultiphaseRankingFunction.non_increasing
    |> entry_transitions program
    |> Base.Set.for_all ~f:(fun (l,t,l') ->
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
                                    "non_increasing", Util.sequence_to_string ~f:Transition.to_id_string (Base.Set.to_sequence (MultiphaseRankingFunction.non_increasing rank));
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

let improve_with_twn program scc conf appr =
  let compute appr_ t =
   let bound = TWN.time_bound conf t scc program appr_ in
   let orginal_bound = get_bound `Time appr_ t in
    if (Bound.compare_asy orginal_bound bound) = 1 then
      MaybeChanged.changed (add_bound `Time bound t appr_)
    else
      MaybeChanged.same appr_ in
  MaybeChanged.fold compute appr (Base.Set.to_list (Base.Set.filter ~f:(Bound.is_infinity % Approximation.timebound appr) scc))

(** Checks if a transition is bounded *)
let bounded measure appr transition =
  match measure with
  | `Time -> Approximation.is_time_bounded appr transition
  | `Cost -> Polynomial.is_const (Transition.cost transition) (* We can not compute a better bound in this case, so we consider this transition as bounded *)

let improve_termination_twn program scc conf appr =
  let compute appr_ t =
    let terminates = TWN.terminates conf t scc program appr_ in
    let orginal_terminates = bounded `Time appr_ t in
    if not orginal_terminates && terminates then
      MaybeChanged.changed (add_bound `Time Bound.one t appr_)
    else
      MaybeChanged.same appr_ in
  MaybeChanged.fold_sequence ~f:compute ~init:appr (Base.Sequence.filter ~f:(Bound.is_infinity % Approximation.timebound appr) (Base.Set.to_sequence scc))


let improve_termination_rank_mprf measure program appr rank =
  let terminates = bounded_mprf program appr rank in
  let orginal_terminates = bounded measure appr (MultiphaseRankingFunction.decreasing rank) in
  if not orginal_terminates && terminates then (
    MultiphaseRankingFunction.add_to_proof rank None program;
    rank
    |> MultiphaseRankingFunction.decreasing
    |> (fun t -> add_bound measure Bound.one t appr)
    |> MaybeChanged.changed)
  else
    MaybeChanged.same appr


let rec complexity_knowledge_propagation (scc: TransitionSet.t) program appr =
  let execute () =
    scc
    |> Base.Set.to_sequence
    |> MaybeChanged.fold_sequence ~f:(
      fun appr transition ->
        let new_bound =
          Program.pre_transitionset_cached program transition
          |> Base.Set.to_sequence
          |> Base.Sequence.map ~f:(Approximation.timebound appr)
          |> Bound.sum_sequence
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
      ) ~init:appr
    |> MaybeChanged.if_changed (complexity_knowledge_propagation scc program)
    |> MaybeChanged.unpack
  in
  (Logger.with_log logger Logger.INFO
        (fun () -> "knowledge prop. ", ["scc", TransitionSet.to_string scc])
         execute)

  let rec termination_knowledge_propagation (scc: TransitionSet.t) program appr =
  let execute () =
    scc
    |> Base.Set.to_sequence
    |> MaybeChanged.fold_sequence ~f:(
      fun appr transition ->
        let terminates =
          Program.pre_transitionset_cached program transition
          |> Base.Set.for_all ~f:(Bound.is_finite % Approximation.timebound appr)
        in
        let original_bounded = bounded `Time appr transition in
        if not original_bounded && terminates then (
          ProofOutput.add_str_paragraph_to_proof (fun () ->
            "knowledge_propagation leads to time bound for transition "^Transition.to_string_pretty transition
          );
          add_bound `Time Bound.one transition appr
          |> MaybeChanged.changed)
        else
            MaybeChanged.same appr
      ) ~init:appr
    |> MaybeChanged.if_changed (termination_knowledge_propagation scc program)
    |> MaybeChanged.unpack
  in
  (Logger.with_log logger Logger.INFO
        (fun () -> "knowledge prop. ", ["scc", TransitionSet.to_string scc])
          execute)

let knowledge_propagation ~conf =
  match conf.analysis_type with
  | `Termination -> termination_knowledge_propagation
  | `Complexity  -> complexity_knowledge_propagation

let local_rank ~conf (scc: TransitionSet.t) measure program max_depth appr =
    let get_unbounded_vars transition =
      match conf.analysis_type with
      | `Termination -> VarSet.empty
      | `Complexity  ->
          program
          |> Program.input_vars
          |> Base.Set.filter ~f:(Bound.is_infinity % Approximation.sizebound appr transition)
    in
    let is_time_bounded = Bound.is_finite % Approximation.timebound appr in
    let unbounded_transitions =
      scc
      |> tap (fun scc -> (Logger.log logger Logger.INFO (fun () -> "improve_timebound", ["scc", TransitionSet.to_string scc])))
      |> Base.Set.filter ~f:(not % bounded measure appr)
    in
    let scc_overapprox_nonlinear = TransitionSet.map ~f:Transition.overapprox_nonlinear_updates scc in
    let rankfunc_computation depth =
      let compute_function trans =
        MultiphaseRankingFunction.find_scc measure program is_time_bounded get_unbounded_vars scc_overapprox_nonlinear depth
          @@ Option.get @@ Base.Set.binary_search scc_overapprox_nonlinear ~compare:Transition.compare `First_equal_to trans
    in
      Base.Set.to_array unbounded_transitions
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
    let improvement_function =
      match conf.analysis_type with
      | `Termination -> improve_termination_rank_mprf
      | `Complexity  -> improve_with_rank_mprf in
    rankfuncs
    |> MaybeChanged.fold_enum (improvement_function measure program) appr


let run_local ~conf (scc: TransitionSet.t) measure program appr =
  MaybeChanged.(
    return appr
    >>= fun appr ->
      (match conf.run_mprf_depth with
        | Some max_depth -> local_rank ~conf scc measure program max_depth appr
        | None           -> MaybeChanged.return appr )
    >>= fun appr ->
      (match measure, conf.twn_configuration, conf.analysis_type with
        | (`Cost,_,_) -> MaybeChanged.return appr
        | (`Time,None,_) -> MaybeChanged.return appr
        | (`Time,Some twn_conf, `Termination) -> improve_termination_twn program scc twn_conf appr
        | (`Time,Some twn_conf, `Complexity)  -> improve_with_twn program scc twn_conf appr)
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
    let Util.TypeEq.Refl = proof_program_is_standard ~eq_proof_prog:Refl                                  in
    let Util.TypeEq.Refl = proof_tset_is_standard    ~eq_proof_trans:Refl ~eq_proof_trans_comparator:Refl in
    TWNSizeBounds.improve program ~scc:(Option.some scc) appr
    |> SolvableSizeBounds.improve program ~scc:(Option.some scc)

(* TODO unify conf types with ~local! *)
let improve_scc ~conf opt_rvg_with_sccs (scc: TransitionSet.t) program opt_lsb_table appr =
  let improvement appr =
    knowledge_propagation ~conf scc program appr
    |> (match conf.analysis_type with
       | `Termination -> identity
       | `Complexity -> fun appr ->
          SizeBounds.improve program (Option.get opt_rvg_with_sccs) ~scc:(Option.some scc) (LSB_Table.find @@ Option.get opt_lsb_table) appr
          |> twn_size_bounds ~conf scc program)
    |> improve_timebound ~conf scc `Time program
  in
  (* First compute initial time bounds for the SCC and then iterate by computing size and time bounds alteratingly *)
  knowledge_propagation ~conf scc program appr
  |> MaybeChanged.unpack % improve_timebound ~conf scc `Time program
  |> Util.find_fixpoint improvement

let scc_cost_bounds ~conf program scc appr =
  if Base.Set.exists ~f:(not % Polynomial.is_const % Transition.cost) scc then
    MaybeChanged.unpack (improve_timebound ~conf scc `Cost program appr)
  else
    appr


let reset_all_caches =
  Program.reset_pre_cache ();
  TWN.reset_cfr();
  TWNSizeBounds.reset_cfr();
  SolvableSizeBounds.reset_cfr()

let handle_timeout_cfr method_name non_linear_transitions =
  reset_all_caches;
  Logger.log logger_cfr Logger.INFO (fun () -> "TIMEOUT_CFR_" ^ method_name, ["scc", (TransitionSet.to_string non_linear_transitions)])


  let all_rvs program input_vars =
    List.cartesian_product
      (Base.Set.to_list (Program.transitions program))
      (Base.Set.to_list input_vars)

  let add_missing_lsbs program lsbs =
    let input_vars = program |> Program.input_vars in
    all_rvs program input_vars
    |> List.iter (fun(t,v)->
        if LSB_Table.mem lsbs (t,v) then ()
        else LSB_Table.add lsbs (t,v) (LSB.compute_bound input_vars t v)
       )

  let compute_lsbs program =
    let vars = program |> Program.input_vars in
    List.enum (all_rvs program vars)
    |> Enum.map (fun(t,v) -> (t,v),LSB.compute_bound vars t v)
    |> LSB_Table.of_enum

  let handle_cfr ~(conf: conf_type) ~(preprocess: Program.t -> Program.t) (scc: TransitionSet.t)
    program opt_rvg opt_lsbs appr: Program.t * Approximation.t * ((RVG.t * RVG.scc list lazy_t) option) =
    match conf.cfr_configuration with
    | NoCFR -> program,appr,opt_rvg
    | PerformCFR cfr -> (
      let Util.TypeEq.Refl = proof_tset_is_standard ~eq_proof_trans:Refl ~eq_proof_trans_comparator:Refl in
      let module Approximation = Approximation_ in
      let module SizeBounds = SizeBounds_ in
      let lsbs = opt_lsbs |? LSB_Table.create 0 in
      let rvg =  opt_rvg |? (RVG.empty,  Lazy.from_fun (const []))  in
      let apply_cfr
          (method_name: string)
          (f_cfr: Program.t -> Program.t MaybeChanged.t)
          (f_proof: Program.t -> string -> unit)
          (rvg_with_sccs: RVGTypes.MakeRVG(ProgramModules).t * RVGTypes.MakeRVG(ProgramModules).scc list Lazy.t)
          (time: float)
          (non_linear_transitions: TransitionSet.t)
          ~(preprocess: Program.t -> Program.t)
          (program: Program.t)
          (appr: Approximation_.MakeForClassicalAnalysis(ProgramModules).t) =

        if Base.Set.is_empty non_linear_transitions then (program, appr, rvg_with_sccs) else
        let mc =
          Logger.log logger_cfr Logger.INFO
            (fun () -> "Analysis_apply_" ^ method_name, [ "non-linear trans", TransitionSet.to_string non_linear_transitions; "time", string_of_float time]);
          f_cfr program in
        if not @@ MaybeChanged.has_changed mc then (program, appr, rvg_with_sccs) else (
          ProofOutput.add_to_proof (fun () -> FormattedString.mk_str_header_big "Analysing control-flow refined program");
          let program_cfr = mc |> MaybeChanged.unpack |> preprocess  in
          if conf.analysis_type == `Complexity then add_missing_lsbs program_cfr lsbs;
          Logger.log logger_cfr Logger.DEBUG (fun () -> "apply_" ^ method_name, []);
          reset_all_caches;
          let rvg_with_sccs_cfr = match conf.analysis_type with
          | `Termination -> RVG.empty, Lazy.from_fun (const [])
          | `Complexity  -> RVG.rvg_with_sccs (Option.map (LSB.vars % Tuple2.first) % LSB_Table.find lsbs) program_cfr in
          (* The new sccs which do not occur in the original program. *)
          let cfr_sccs =
            Program.sccs program_cfr
            |> List.filter (fun cfr_scc -> not (Base.List.exists ~f:(fun scc_ -> Base.Set.equal cfr_scc scc_) (Program.sccs program))) in
          let update_appr appr scc = match conf.analysis_type with
          | _ when not @@ Base.Set.exists ~f:(Bound.is_infinity % Approximation.timebound appr) scc -> appr
          | `Termination ->
              Logger.log logger Logger.INFO (fun () -> method_name ^ "analysis", ["scc", TransitionSet.to_id_string scc]);
              improve_scc ~conf (Some rvg_with_sccs_cfr) scc program_cfr opt_lsbs appr
          | `Complexity  -> (
              Logger.log logger Logger.INFO (fun () -> method_name ^ "analysis", ["scc", TransitionSet.to_id_string scc]);
              SizeBounds.improve program_cfr rvg_with_sccs_cfr ~scc:(Option.some scc) (LSB_Table.find lsbs) appr
              |> twn_size_bounds ~conf scc program_cfr
              |> improve_scc ~conf (Some rvg_with_sccs_cfr) scc program_cfr opt_lsbs)
          in
          let updated_appr_cfr =
            cfr_sccs
            |> List.fold_left update_appr (CFR.merge_appr program program_cfr appr) in
          let handle_no_improvement org_bound cfr_bound =
            ProofOutput.add_to_proof (fun () -> FormattedString.mk_str_header_big "CFR did not improve the program. Rolling back");
            reset_all_caches;
            Logger.log logger_cfr Logger.INFO (fun () -> "NOT_IMPROVED",
                                                        ["original bound", org_bound; method_name ^ " bound", cfr_bound]);
            (program, appr, rvg_with_sccs)
          in
          let handle_cfr_termination =
            let org_terminates = Base.Set.for_all ~f:(Bound.is_finite % Approximation.timebound appr) scc in
            let cfr_terminates = List.for_all
              (fun scc -> Base.Set.for_all ~f:(Bound.is_finite % Approximation.timebound updated_appr_cfr) scc) cfr_sccs in
            let show b = if b then "Finite" else "Infinite" in (* For Debugging and Proofs *)
            if org_terminates || not cfr_terminates then
              handle_no_improvement (show org_terminates) (show cfr_terminates)
            else
              (f_proof program_cfr @@ show org_terminates;
              program_cfr, updated_appr_cfr, rvg_with_sccs_cfr)
          in
          (*Calculates concrete bounds*)
          let handle_cfr_complexity =
            let org_bound = Bound.sum_sequence (Base.Sequence.map ~f:(Approximation.timebound appr) (Base.Set.to_sequence scc))  in
            let cfr_bound = Bound.sum (Enum.map
                                      (fun scc -> Bound.sum_sequence (Base.Sequence.map ~f:(Approximation.timebound updated_appr_cfr) (Base.Set.to_sequence scc)))
                                      (List.enum cfr_sccs))  in
            if (Bound.compare_asy org_bound cfr_bound) < 1 then handle_no_improvement (Bound.to_string ~pretty:true org_bound) (Bound.show_complexity @@ Bound.asymptotic_complexity cfr_bound) else
              (f_proof program_cfr @@ Bound.to_string ~pretty:true cfr_bound;
              program_cfr, updated_appr_cfr, rvg_with_sccs_cfr)
          in
          match conf.analysis_type with
          | `Termination -> handle_cfr_termination
          | `Complexity  -> handle_cfr_complexity)
      in
      let partial_evaluation (program, appr, rvg) =
          let non_linear_transitions =
            Base.Set.filter ~f:(not % Bound.is_linear % Approximation.timebound appr) (Base.Set.inter scc (Program.transitions program))
          in
          let time = PartialEvaluation.compute_timeout_time program appr scc in
          Timeout.timed_run time ~action:(fun () -> handle_timeout_cfr "partial_evaluation" scc)
              (fun () -> apply_cfr "partial_evaluation" (PartialEvaluation.apply_cfr non_linear_transitions) (PartialEvaluation.add_to_proof) rvg time non_linear_transitions ~preprocess program appr)
          |> Option.map_default
            (fun (res, time) -> PartialEvaluation.time_cfr := !PartialEvaluation.time_cfr -. time; res)
            (program, appr, rvg)
      in
      let non_linear_transitions =
        Base.Set.filter ~f:(not % Bound.is_linear % Approximation.timebound appr) scc
      in
        let chaining =
        Timeout.timed_run 10. ~action:(fun () -> handle_timeout_cfr "partial_evaluation" scc)
            (fun () -> apply_cfr "chaining" (CFR.lift_to_program (Chaining.transform_graph ~scc:(Option.some scc))) (fun _ _ -> ()) rvg 10. non_linear_transitions ~preprocess program appr)
        |> Option.map_default Tuple2.first (program, appr, rvg)
      in
      if not (Base.Set.is_empty non_linear_transitions) && List.mem `Chaining cfr then
        chaining
      else (program, appr, rvg)
      |> if !PartialEvaluation.time_cfr > 0. && not (Base.Set.is_empty non_linear_transitions) && List.mem `PartialEvaluation cfr then
          partial_evaluation
        else identity
    )
    |> Tuple3.map3 Option.some

  let improve ~conf ~preprocess program appr =
    (* let appr = List.fold (fun appr (v,t) -> Approximation.add_sizebound (Bound.of_var v) t v appr) appr (List.cartesian_product (Program.input_vars program |> VarSet.to_list) (Program.transitions program |> TransitionSet.to_list)) in *) (*TODO Remove and make unhacky (heuristic entry transition terminates instead of sizebounds)*)
    let opt_lsbs = match conf.analysis_type with
      |`Termination -> None
      |`Complexity  -> Option.some @@ compute_lsbs program in
    let opt_rvg = match conf.analysis_type with
      |`Termination -> None
      |`Complexity  -> Option.some @@ RVG.rvg_with_sccs (Option.map (LSB.vars % Tuple2.first) % LSB_Table.find (Option.get opt_lsbs)) program in
    let program, appr =
      program
      |> Program.sccs
      |> List.fold_left (fun (program, appr, rvg) scc_orig ->
             let improve_scc scc =
               if Base.Set.exists ~f:(fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc then
                 appr
                 |> tap (const @@ Logger.log logger Logger.INFO (fun () -> "continue analysis", ["scc", TransitionSet.to_id_string scc]))
                 |> (match conf.analysis_type with
                    | `Termination -> identity
                    | `Complexity  -> fun appr ->
                      SizeBounds.improve program (Option.get opt_rvg) ~scc:(Option.some scc) (LSB_Table.find (Option.get opt_lsbs)) appr
                      |> twn_size_bounds ~conf scc program)
                 |> improve_scc ~conf opt_rvg scc program opt_lsbs
                 (* Apply CFR if requested; timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
                 |> handle_cfr ~conf ~preprocess scc program opt_rvg opt_lsbs
                 |> fun (program,appr,opt_rvg) -> (program, scc_cost_bounds ~conf program scc appr,opt_rvg)
               else (program, appr, opt_rvg)
             in
             (* Check if SCC still exists and keep only existing transitions (Preprocessing in cfr might otherwise cut them ) *)
             match conf.cfr_configuration with
             | NoCFR        -> improve_scc scc_orig
             | PerformCFR _ ->
                let scc = Base.Set.inter scc_orig (Program.transitions program) in
                if Base.Set.is_empty scc then (program,appr,rvg)
                else improve_scc scc
           ) (program, appr, opt_rvg)
      |> Tuple3.get12
    in
    program, match conf.analysis_type with
              | `Termination -> appr
              | `Complexity -> CostBounds.infer_from_timebounds program appr
end
