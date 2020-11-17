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

type measure = [ `Cost | `Time ] [@@deriving show, eq]

exception NOT_IMPROVED

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
let compute_bound_mprf (appr: Approximation.t) (program: Program.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> Program.entry_transitions logger program
   |> List.enum
   |> Enum.map (fun (l,t,l') ->
       let timebound = Approximation.timebound appr (l,t,l') in 
       let evaluate = (fun rank -> (apply (fun kind -> Approximation.sizebound kind appr) rank) (l,t,l')) in
       let evaluated_rankingFunctions = (List.init (MultiphaseRankingFunction.depth rank) (fun i -> (evaluate ((List.nth (MultiphaseRankingFunction.rank rank) i) l')))) in
       let rhs = if MultiphaseRankingFunction.depth rank = 1 then
          Bound. (max Bound.zero (add Bound.one (List.nth evaluated_rankingFunctions 0)))
          else 
          Bound. (add Bound.one (mul (of_int (MPRF_Coefficient.coefficient rank))  (MPRF_Coefficient.maxBound_of_list evaluated_rankingFunctions))) in 
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
    if not (Bound.is_linear bound) && not (IDSet.mem (Transition.id (MultiphaseRankingFunction.decreasing rank)) !CFR.already_used_cfr) then
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
     |> Program.entry_transitions logger program
     |> List.enum
     |> Enum.map (fun (l,t,l') ->
            let timebound = Approximation.timebound appr (l,t,l') in
            let rhs = Bound.(max zero (apply (fun kind -> Approximation.sizebound kind appr) (RankingFunction.rank rank l') (l,t,l'))) in
            let p0 = Polynomial.of_coeff_list [OurInt.of_int (-21)] [Var.of_string "Arg_0"] in
            let p1 = Polynomial.add (Polynomial.of_int 420) p0 in
            let appr1 = Approximation.add_sizebound `Lower Bound.one (l,t,l') (Var.of_string "Arg_0") (Approximation.create program) in
            let appr2 = Approximation.add_sizebound `Upper (Bound.infinity) (l,t,l') (Var.of_string "Arg_0") appr1 in
            Printf.printf "Poly/RF: %S, appr_upper: %S" (Polynomial.to_string p1) 
                                                (Bound.to_string 
                                                (Bound.appr_substitution 
                                                  `Upper 
                                                  ~lower:(fun var -> (Approximation.sizebound `Lower appr2 (l,t,l') var))
                                                  ~higher:(fun var -> (Approximation.sizebound `Upper appr2 (l,t,l') var))
                                                  (Bound.of_poly p1)));
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
    if not (Bound.is_linear bound) && not (IDSet.mem (Transition.id (RankingFunction.decreasing rank)) !CFR.already_used_cfr)  then 
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

let improve_with_rank_mprf measure program appr rank =
  let bound = compute_bound_mprf appr program rank in
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
let rec improve_timebound_rec ?(mprf = false) ?(inv = false) (scc: TransitionSet.t)  measure program appr =
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
            MultiphaseRankingFunction.find_scc ~inv:inv measure (Option.is_some !backtrack_point) program transition scc
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mprf measure program appr rank) appr
          else 
            RankingFunction.find_scc ~inv:inv measure (Option.is_some !backtrack_point) program transition scc                     
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank) appr)
      ) appr in 
      (Logger.with_log logger Logger.INFO
            (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
             execute)
      |> MaybeChanged.if_changed (improve_timebound_rec ~mprf:mprf ~inv:inv scc measure program)
      |> MaybeChanged.unpack

  let improve_timebound ?(mprf = false) ?(inv = false) (scc: TransitionSet.t)  measure program appr =
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
            MultiphaseRankingFunction.find_scc ~inv:inv measure (Option.is_some !backtrack_point) program transition scc
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mprf measure program appr rank) appr
          else 
            RankingFunction.find_scc  ~inv:inv measure (Option.is_some !backtrack_point) program transition scc                    
            |> List.enum
            |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank) appr)
      ) appr in 
      (Logger.with_log logger Logger.INFO
            (fun () -> "improve_bounds", ["scc", TransitionSet.to_string scc; "measure", show_measure measure])
             execute)

  let rec improve_scc ?(mprf = false) ?(inv = false) (scc: TransitionSet.t)  measure program appr = 
    appr
    |> improve_timebound_rec ~mprf:mprf ~inv:inv scc measure program
    |> SizeBounds.improve program ~scc:(Option.some scc) (Option.is_some !backtrack_point)
    |> improve_timebound ~mprf:mprf ~inv:inv scc measure program
    |> MaybeChanged.if_changed (improve_scc ~mprf:mprf ~inv:inv scc measure program)
    |> MaybeChanged.unpack


let apply_cfr ?(cfr = false) ?(mprf = false) (scc: TransitionSet.t) measure program appr =
  if Option.is_some !backtrack_point then (
    (** Done with orginial component and set backtrack_point *)
    let (org_program,_,_) = Option.get !backtrack_point in
    if not (TransitionSet.disjoint (Program.transitions org_program) scc) then 
      backtrack_point := None
  );
  if Option.is_some !backtrack_point then (
    let (_,_,org_bound) = Option.get !backtrack_point in
    let cfr_bound = Bound.sum (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
    if (Bound.compare_asy org_bound cfr_bound) < 1 then (
      Logger.log logger_cfr Logger.INFO (fun () -> "NOT_IMPROVED", ["orginial bound", (Bound.to_string org_bound); "cfr bound", (Bound.show_complexity (Bound.asymptotic_complexity cfr_bound))]);
      raise NOT_IMPROVED
    )
  ); 
  if cfr && not (TransitionSet.is_empty !CFR.nonLinearTransitions)  then
      let org_bound = Bound.sum
       (Enum.map (fun t -> Approximation.timebound appr t) (TransitionSet.enum scc))  in
        backtrack_point := Option.some (program,appr,org_bound);
      let opt = 
        CFR.set_time_current_cfr scc appr;
        CFR.number_unsolved_trans := !CFR.number_unsolved_trans - (TransitionSet.cardinal scc);
        Logger.log logger_cfr Logger.INFO (fun () -> "RankingBounds", ["non-linear trans", (TransitionSet.to_string !nonLinearTransitions); "time", string_of_float !CFR.time_current_cfr]);
        CFR.apply_cfr program appr in
      if Option.is_some opt then (
      let (program_cfr,appr_cfr) = Option.get opt in
      backtrack_point := Option.some (program,appr,org_bound);
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

let rec improve ?(mprf = false) ?(cfr = false) ?(inv = false) measure program appr =
  program
    |> Program.sccs
    |> List.of_enum
    |> fold_until (fun monad scc -> 
                        if (TransitionSet.exists (fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc) then (
                          if mprf then 
                            MultiphaseRankingFunction.reset()
                          else
                            RankingFunction.reset(); 
                          try 
                            appr               
                            |> SizeBounds.improve program ~scc:(Option.some scc) (Option.is_some !backtrack_point)
                            |> improve_scc ~mprf:mprf ~inv:inv scc measure program
                            |> apply_cfr ~cfr:cfr ~mprf:mprf scc measure program
                          with TIMEOUT | NOT_IMPROVED ->
                            LocalSizeBound.reset_cfr ();  
                            let (program,appr,_) = Option.get !backtrack_point in
                            backtrack_point := None;
                            MaybeChanged.changed (program,appr))
                        else monad) 
                  (fun monad -> MaybeChanged.has_changed monad) (MaybeChanged.same (program,appr))
    |> MaybeChanged.if_changed (fun (a,b) -> (improve ~cfr:cfr ~mprf:mprf measure a b))
    |> MaybeChanged.unpack